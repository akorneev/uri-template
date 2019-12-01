package com.github.akorneev.uritemplate

import java.nio.CharBuffer
import java.nio.charset.StandardCharsets

import fastparse.NoWhitespace._
import fastparse._

import scala.annotation.{switch, tailrec}

sealed trait Template
case class CompiledTemplate(private[uritemplate] val parts: Seq[TemplatePart]) extends Template
case class StringTemplate(template: String)                                    extends Template

sealed trait TemplateParam {
  def name: String
}
case class StringParam(name: String, value: String)            extends TemplateParam
case class ListParam(name: String, values: Seq[String])        extends TemplateParam
case class MapParam(name: String, values: Map[String, String]) extends TemplateParam
case class UndefParam(name: String)                            extends TemplateParam

case class TemplateError[A](message: String, details: Option[A] = None) extends RuntimeException(message)

private[uritemplate] case class VarName(name: String) extends AnyVal

sealed private[uritemplate] trait Modifier
private[uritemplate] case class MaxLength(length: Int) extends Modifier
private[uritemplate] case object Explode               extends Modifier

private[uritemplate] case class VarSpec(name: VarName, modifier: Option[Modifier] = None)
private[uritemplate] case class VarList(vars: Seq[VarSpec]) extends AnyVal
private[uritemplate] case class Operator(name: String)      extends AnyVal

sealed private[uritemplate] trait TemplatePart
private[uritemplate] case class LiteralChar(value: Char)                              extends TemplatePart
private[uritemplate] case class EncodedString(value: String)                          extends TemplatePart
private[uritemplate] case class Expression(vars: VarList, operator: Option[Operator]) extends TemplatePart

object Template {
  def apply(s: String): StringTemplate = StringTemplate(s)

  def compile(s: String): CompiledTemplate =
    fastparse.parse(s, parser) match {
      case Parsed.Success(value, _)    => optimize(value)
      case f @ Parsed.Failure(_, _, _) => throw TemplateError(s"Cannot parse URI Template: $s", Some(f.trace().stack))
    }

  @tailrec
  def expand(template: Template, params: Seq[TemplateParam]): String = template match {
    case StringTemplate(template) => expand(compile(template), params)
    case CompiledTemplate(parts) =>
      parts.map {
        case LiteralChar(c)   => encodeAllowReserved(c)
        case EncodedString(s) => s
        case Expression(vs, Some(operator)) =>
          (operator: @unchecked) match {
            case Operator("+") => expandVars(vs.vars, params, prefix = "", separator = ",", encodeFn = encodeAllowReserved)
            case Operator("#") => expandVars(vs.vars, params, prefix = "#", separator = ",", encodeFn = encodeAllowReserved)
            case Operator(".") => expandVars(vs.vars, params, prefix = ".", separator = ".", encodeFn = encodeAllowUnreserved)
            case Operator("/") => expandVars(vs.vars, params, prefix = "/", separator = "/", encodeFn = encodeAllowUnreserved)
            case Operator(";") => expandKvVars(vs.vars, params, prefix = ";", separator = ";", omitAssignment = true, encodeFn = encodeAllowUnreserved)
            case Operator("?") => expandKvVars(vs.vars, params, prefix = "?", separator = "&", omitAssignment = false, encodeFn = encodeAllowUnreserved)
            case Operator("&") => expandKvVars(vs.vars, params, prefix = "&", separator = "&", omitAssignment = false, encodeFn = encodeAllowUnreserved)
          }
        case Expression(vs, None) => expandVars(vs.vars, params, prefix = "", separator = ",", encodeFn = encodeAllowUnreserved)
      }.mkString
  }

  private val parser: P[_] => P[CompiledTemplate] = {
    def alpha[_: P]             = CharIn("a-zA-Z").!
    def digit[_: P]             = CharIn("0-9").!
    def hexdig[_: P]            = digit | CharIn("a-fA-F").!
    def `pct-encoded`[_: P]     = ("%".! ~ hexdig.rep(exactly = 2)).map { case (c, cs) => (c +: cs).mkString }
    def `uri-template`[_: P]    = ((literal | expression).rep map (CompiledTemplate(_))) ~ End
    def literalChar[_: P]       = CharPred(isLiteralChar).! map (c => LiteralChar(c(0)))
    def literal[_: P]           = literalChar | (`pct-encoded` map EncodedString)
    def expression[_: P]        = ("{" ~ operator.? ~ `variable-list` ~ "}") map { case (op, vars) => Expression(vars, op) }
    def operator[_: P]          = (`op-level2` | `op-level3` | `op-reserve`).map(Operator)
    def `op-level2`[_: P]       = CharIn("+#").!
    def `op-level3`[_: P]       = CharIn("./;?&").!
    def `op-reserve`[_: P]      = CharIn("=,!@|").!
    def `variable-list`[_: P]   = varspec.rep(min = 1, sep = ",") map VarList
    def varspec[_: P]           = varname ~ `modifier-level4`.? map { case (n, m) => VarSpec(n, m) }
    def varname[_: P]           = `varname-part`.rep(min = 1, sep = ".".?) map (parts => VarName(parts.mkString))
    def `varname-part`[_: P]    = (varchar ~ varchar.rep) map { case (c, cs) => (c +: cs).mkString }
    def varchar[_: P]           = alpha | digit | "_".! | `pct-encoded`
    def `modifier-level4`[_: P] = prefix | explode
    def prefix[_: P]            = ":" ~ `max-length` map (s => MaxLength(Integer.parseInt(s)))
    def `max-length`[_: P]      = CharIn("1-9").! ~ digit.rep(min = 0, max = 3) map { case (c, cs) => (c +: cs).mkString }
    def explode[_: P]           = "*".! map (_ => Explode)

    `uri-template`(_)
  }

  private def compress(ps: Seq[TemplatePart]): EncodedString =
    EncodedString(ps.map {
      case LiteralChar(c)   => c.toString
      case EncodedString(s) => s
      case Expression(_, _) => sys.error("Unexpected expression.")
    }.mkString)

  private def optimize(template: CompiledTemplate): CompiledTemplate = {
    @tailrec
    def loop(ps: List[TemplatePart], opt: Seq[TemplatePart], acc: Seq[TemplatePart]): Seq[TemplatePart] = ps match {
      case Nil if acc.nonEmpty                     => opt :+ compress(acc)
      case Nil                                     => opt
      case (p: LiteralChar) :: rest                => loop(rest, opt, acc :+ p)
      case (p: EncodedString) :: rest              => loop(rest, opt, acc :+ p)
      case (p: Expression) :: rest if acc.nonEmpty => loop(rest, opt :+ compress(acc) :+ p, Seq.empty)
      case (p: Expression) :: rest                 => loop(rest, opt :+ p, Seq.empty)
    }
    CompiledTemplate(loop(template.parts.toList, Seq.empty, Seq.empty))
  }

  private def getParam(name: String, params: Seq[TemplateParam]): Option[TemplateParam] = params find (_.name == name)

  private def encodeChar(c: Char): String = {
    val charset = StandardCharsets.UTF_8
    val buf     = CharBuffer.allocate(1)
    buf.put(0, c)
    charset.encode(buf).array().map("%%%02X" format _).mkString
  }

  private def encodeAllowReserved(c: Char): String = (c: @switch) match {
    // format: off
    case 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' |
         's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' |
         'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' | '0' | '1' |
         '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' | '.' | '_' | '~' | ':' | '/' | '?' | '#' | '[' | ']' |
         '@' | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' => c.toString
    case _ => encodeChar(c)
    // format: on
  }

  private def encodeAllowUnreserved(c: Char): String = (c: @switch) match {
    // format: off
    case 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' |
         's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' |
         'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' | '0' | '1' |
         '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' | '.' | '_' | '~'  => c.toString
    case _ => encodeChar(c)
    // format: on
  }

  private def encodeAllowUnreserved(s: String): String = s flatMap (encodeAllowUnreserved(_))

  private def encodeAllowReserved(s: String): String = s flatMap (encodeAllowReserved(_))

  private def expandVars(vars: Seq[VarSpec], params: Seq[TemplateParam], prefix: String, separator: String, encodeFn: String => String): String =
    vars match {
      case vs if vs.nonEmpty =>
        val expandedVars = vs map {
          case VarSpec(VarName(name), modifier) =>
            getParam(name, params) match {
              case Some(StringParam(_, value)) =>
                Some(modifier match {
                  case Some(Explode) | None       => encodeFn(value)
                  case Some(MaxLength(maxLength)) => encodeFn(value take maxLength)
                })
              case Some(ListParam(_, values)) =>
                Some(modifier match {
                  case Some(Explode)          => values map encodeFn mkString separator
                  case None                   => values map encodeFn mkString ","
                  case Some(m @ MaxLength(_)) => throw TemplateError("Unexpected modifier.", Some(m))
                })
              case Some(MapParam(_, values)) =>
                if (values.nonEmpty) {
                  Some(modifier match {
                    case Some(Explode)          => values map { case (k, v) => s"$k=${encodeFn(v)}" } mkString separator
                    case None                   => simpleExpandMapValues(values, encodeFn)
                    case Some(m @ MaxLength(_)) => throw TemplateError("Unexpected modifier.", Some(m))
                  })
                } else None
              case Some(UndefParam(_)) => None
              case None                => None
            }
        } filter (_.nonEmpty)
        if (expandedVars.nonEmpty) prefix + (expandedVars.flatten mkString separator)
        else ""
    }

  private def expandKvVars(
      vars: Seq[VarSpec],
      params: Seq[TemplateParam],
      prefix: String,
      separator: String,
      omitAssignment: Boolean,
      encodeFn: String => String
  ): String =
    vars match {
      case vs if vs.nonEmpty =>
        val expandedVars = vs map {
          case VarSpec(VarName(name), modifier) =>
            getParam(name, params) match {
              case Some(StringParam(_, "")) => Some(if (omitAssignment) name else s"$name=")
              case Some(StringParam(_, value)) =>
                Some(modifier match {
                  case Some(Explode) | None       => s"$name=${encodeFn(value)}"
                  case Some(MaxLength(maxLength)) => s"$name=${encodeFn(value take maxLength)}"
                })
              case Some(ListParam(_, values)) =>
                Some(modifier match {
                  case Some(Explode) =>
                    values map {
                      case ""    => if (omitAssignment) name else s"$name="
                      case value => s"$name=${encodeFn(value)}"
                    } mkString separator
                  case None =>
                    name + "=" + (values filter (_.nonEmpty) map encodeFn mkString ",")
                  case Some(m @ MaxLength(_)) => throw TemplateError("Unexpected modifier.", Some(m))
                })
              case Some(MapParam(_, values)) =>
                if (values.nonEmpty) {
                  Some(modifier match {
                    case Some(Explode) =>
                      values map {
                        case (k, "") => if (omitAssignment) k else s"$k="
                        case (k, v)  => s"$k=${encodeFn(v)}"
                      } mkString separator
                    case None                   => name + "=" + simpleExpandMapValues(values, encodeFn)
                    case Some(m @ MaxLength(_)) => throw TemplateError("Unexpected modifier.", Some(m))
                  })
                } else None
              case Some(UndefParam(_)) => None
              case None                => None
            }
        } filter (_.nonEmpty)
        if (expandedVars.nonEmpty) prefix + (expandedVars.flatten mkString separator)
        else ""
    }

  private def simpleExpandMapValues(values: Map[String, String], encodeFn: String => String) =
    values flatMap { case (k, v) => Seq(k, encodeFn(v)) } filter (_.nonEmpty) mkString ","

  private def isLiteralChar(c: Char): Boolean = (c: @switch) match {
    case Character.CONTROL | ' ' | '\'' | '%' | '<' | '>' | '\\' | '^' | '`' | '{' | '|' | '}' => false
    case _                                                                                     => true
  }
}

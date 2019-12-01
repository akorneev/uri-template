package com.github.akorneev.uritemplate

import org.scalatest.{FreeSpec, Matchers}

class UriTemplateSpec extends FreeSpec with Matchers {
  "UriTemplate" - {
    "should handle all examples from  RFC 6570" - {
      "Level 1 examples" - {
        val vars = Seq(StringParam("var", "value"), StringParam("hello", "Hello World!"))
        "Simple string expansion" - {
          "{var} => value" in {
            Template.expand(Template("{var}"), vars) should equal("value")
          }
          "{hello} => Hello%20World%21" in {
            Template.expand(Template("{hello}"), vars) should equal("Hello%20World%21")
          }
        }
      }
      "Level 2 examples" - {
        val vars = Seq(StringParam("var", "value"), StringParam("hello", "Hello World!"), StringParam("path", "/foo/bar"))
        "+ Reserved string expansion" - {
          "{+var} => value" in {
            Template.expand(Template("{+var}"), vars) should equal("value")
          }
          "{+hello} => Hello%20World!" in {
            Template.expand(Template("{+hello}"), vars) should equal("Hello%20World!")
          }
          "{+path}/here => /foo/bar/here" in {
            Template.expand(Template("{+path}/here"), vars) should equal("/foo/bar/here")
          }
          "here?ref={+path} => here?ref=/foo/bar" in {
            Template.expand(Template("here?ref={+path}"), vars) should equal("here?ref=/foo/bar")
          }
        }
        "# Fragment expansion, crosshatch-prefixed" - {
          "X{#var} => X#value" in {
            Template.expand(Template("X{#var}"), vars) should equal("X#value")
          }
          "X{#hello} => X#Hello%20World!" in {
            Template.expand(Template("X{#hello}"), vars) should equal("X#Hello%20World!")
          }
        }
      }
      "Level 3 examples" - {
        val vars = Seq(
          StringParam("var", "value"),
          StringParam("hello", "Hello World!"),
          StringParam("empty", ""),
          StringParam("path", "/foo/bar"),
          StringParam("x", "1024"),
          StringParam("y", "768")
        )
        "String expansion with multiple variables" - {
          "map?{x,y} => map?1024,768" in {
            Template.expand(Template("map?{x,y}"), vars) should equal("map?1024,768")
          }
          "{x,hello,y} => 1024,Hello%20World%21,768" in {
            Template.expand(Template("{x,hello,y}"), vars) should equal("1024,Hello%20World%21,768")
          }
        }
        "+ Reserved expansion with multiple variables" - {
          "{+x,hello,y} => 1024,Hello%20World!,768" in {
            Template.expand(Template("{+x,hello,y}"), vars) should equal("1024,Hello%20World!,768")
          }
          "{+path,x}/here => /foo/bar,1024/here" in {
            Template.expand(Template("{+path,x}/here"), vars) should equal("/foo/bar,1024/here")
          }
        }
        "# Fragment expansion with multiple variables" - {
          "{#x,hello,y} => #1024,Hello%20World!,768" in {
            Template.expand(Template("{#x,hello,y}"), vars) should equal("#1024,Hello%20World!,768")
          }
          "{#path,x}/here => #/foo/bar,1024/here" in {
            Template.expand(Template("{#path,x}/here"), vars) should equal("#/foo/bar,1024/here")
          }
        }
        ". Label expansion, dot-prefixed" - {
          "X{.var} => X.value" in {
            Template.expand(Template("X{.var}"), vars) should equal("X.value")
          }
          "X{.x,y} => X.1024.768" in {
            Template.expand(Template("X{.x,y}"), vars) should equal("X.1024.768")
          }
        }
        "/ Path segments, slash-prefixed" - {
          "{/var} => /value" in {
            Template.expand(Template("{/var}"), vars) should equal("/value")
          }
          "{/var,x}/here => /value/1024/here" in {
            Template.expand(Template("{/var,x}/here"), vars) should equal("/value/1024/here")
          }
        }
        "; Path-style parameters, semicolon-prefixed" - {
          "{;x,y} => ;x=1024;y=768" in {
            Template.expand(Template("{;x,y}"), vars) should equal(";x=1024;y=768")
          }
          "{;x,y,empty} => ;x=1024;y=768;empty" in {
            Template.expand(Template("{;x,y,empty}"), vars) should equal(";x=1024;y=768;empty")
          }
        }
        "? Form-style query, ampersand-separated" - {
          "{?x,y} => ?x=1024&y=768" in {
            Template.expand(Template("{?x,y}"), vars) should equal("?x=1024&y=768")
          }
          "{?x,y,empty} => ?x=1024&y=768&empty=" in {
            Template.expand(Template("{?x,y,empty}"), vars) should equal("?x=1024&y=768&empty=")
          }
        }
        "& Form-style query continuation" - {
          "?fixed=yes{&x} => ?fixed=yes&x=1024" in {
            Template.expand(Template("?fixed=yes{&x}"), vars) should equal("?fixed=yes&x=1024")
          }
          "{&x,y,empty} => &x=1024&y=768&empty=" in {
            Template.expand(Template("{&x,y,empty}"), vars) should equal("&x=1024&y=768&empty=")
          }
        }
      }
      "Level 4 examples" - {
        val vars = Seq(
          StringParam("var", "value"),
          StringParam("hello", "Hello World!"),
          StringParam("path", "/foo/bar"),
          ListParam("list", Seq("red", "green", "blue")),
          MapParam("keys", Map("semi" -> ";", "dot" -> ".", "comma" -> ","))
        )
        "String expansion with value modifiers" - {
          "{var:3} => val" in {
            Template.expand(Template("{var:3}"), vars) should equal("val")
          }
          "{var:30} => value" in {
            Template.expand(Template("{var:30}"), vars) should equal("value")
          }
          "{list} => red,green,blue" in {
            Template.expand(Template("{list}"), vars) should equal("red,green,blue")
          }
          "{list*} => red,green,blue" in {
            Template.expand(Template("{list*}"), vars) should equal("red,green,blue")
          }
          "{keys} => semi,%3B,dot,.,comma,%2C" in {
            Template.expand(Template("{keys}"), vars) should equal("semi,%3B,dot,.,comma,%2C")
          }
          "{keys*} => semi=%3B,dot=.,comma=%2C" in {
            Template.expand(Template("{keys*}"), vars) should equal("semi=%3B,dot=.,comma=%2C")
          }
        }
        "+ Reserved expansion with value modifiers" - {
          "{+path:6}/here => /foo/b/here" in {
            Template.expand(Template("{+path:6}/here"), vars) should equal("/foo/b/here")
          }
          "{+list} => red,green,blue" in {
            Template.expand(Template("{+list}"), vars) should equal("red,green,blue")
          }
          "{+list*} => red,green,blue" in {
            Template.expand(Template("{+list*}"), vars) should equal("red,green,blue")
          }
          "{+keys} => semi,;,dot,.,comma,," in {
            Template.expand(Template("{+keys}"), vars) should equal("semi,;,dot,.,comma,,")
          }
          "{+keys*} => semi=;,dot=.,comma=," in {
            Template.expand(Template("{+keys*}"), vars) should equal("semi=;,dot=.,comma=,")
          }
        }
        "# Fragment expansion with value modifiers" - {
          "{#path:6}/here => #/foo/b/here" in {
            Template.expand(Template("{#path:6}/here"), vars) should equal("#/foo/b/here")
          }
          "{#list} => #red,green,blue" in {
            Template.expand(Template("{#list}"), vars) should equal("#red,green,blue")
          }
          "{#list*} => #red,green,blue" in {
            Template.expand(Template("{#list*}"), vars) should equal("#red,green,blue")
          }
          "{#keys} => #semi,;,dot,.,comma,," in {
            Template.expand(Template("{#keys}"), vars) should equal("#semi,;,dot,.,comma,,")
          }
          "{#keys*} => #semi=;,dot=.,comma=," in {
            Template.expand(Template("{#keys*}"), vars) should equal("#semi=;,dot=.,comma=,")
          }
        }
        ". Label expansion, dot-prefixed" - {
          "X{.var:3} => X.val" in {
            Template.expand(Template("X{.var:3}"), vars) should equal("X.val")
          }
          "X{.list} => X.red,green,blue" in {
            Template.expand(Template("X{.list}"), vars) should equal("X.red,green,blue")
          }
          "X{.list*} => X.red.green.blue" in {
            Template.expand(Template("X{.list*}"), vars) should equal("X.red.green.blue")
          }
          "X{.keys} => X.semi,%3B,dot,.,comma,%2C" in {
            Template.expand(Template("X{.keys}"), vars) should equal("X.semi,%3B,dot,.,comma,%2C")
          }
          "X{.keys*} => X.semi=%3B.dot=..comma=%2C" in {
            Template.expand(Template("X{.keys*}"), vars) should equal("X.semi=%3B.dot=..comma=%2C")
          }
        }
        "/ Path segments, slash-prefixed" - {
          "{/var:1,var} => /v/value" in {
            Template.expand(Template("{/var:1,var}"), vars) should equal("/v/value")
          }
          "{/list} => /red,green,blue" in {
            Template.expand(Template("{/list}"), vars) should equal("/red,green,blue")
          }
          "{/list*} => /red/green/blue" in {
            Template.expand(Template("{/list*}"), vars) should equal("/red/green/blue")
          }
          "{/list*,path:4} => /red/green/blue/%2Ffoo" in {
            Template.expand(Template("{/list*,path:4}"), vars) should equal("/red/green/blue/%2Ffoo")
          }
          "{/keys} => /semi,%3B,dot,.,comma,%2C" in {
            Template.expand(Template("{/keys}"), vars) should equal("/semi,%3B,dot,.,comma,%2C")
          }
          "{/keys*} => /semi=%3B/dot=./comma=%2C" in {
            Template.expand(Template("{/keys*}"), vars) should equal("/semi=%3B/dot=./comma=%2C")
          }
        }
        "; Path-style parameters, semicolon-prefixed" - {
          "{;hello:5} => ;hello=Hello" in {
            Template.expand(Template("{;hello:5}"), vars) should equal(";hello=Hello")
          }
          "{;list} => ;list=red,green,blue" in {
            Template.expand(Template("{;list}"), vars) should equal(";list=red,green,blue")
          }
          "{;list*} => ;list=red;list=green;list=blue" in {
            Template.expand(Template("{;list*}"), vars) should equal(";list=red;list=green;list=blue")
          }
          "{;keys} => ;keys=semi,%3B,dot,.,comma,%2C" in {
            Template.expand(Template("{;keys}"), vars) should equal(";keys=semi,%3B,dot,.,comma,%2C")
          }
          "{;keys*} => ;semi=%3B;dot=.;comma=%2C" in {
            Template.expand(Template("{;keys*}"), vars) should equal(";semi=%3B;dot=.;comma=%2C")
          }
        }
        "? Form-style query, ampersand-separated" - {
          "{?var:3} => ?var=val" in {
            Template.expand(Template("{?var:3}"), vars) should equal("?var=val")
          }
          "{?list} => ?list=red,green,blue" in {
            Template.expand(Template("{?list}"), vars) should equal("?list=red,green,blue")
          }
          "{?list*} => ?list=red&list=green&list=blue" in {
            Template.expand(Template("{?list*}"), vars) should equal("?list=red&list=green&list=blue")
          }
          "{?keys} => ?keys=semi,%3B,dot,.,comma,%2C" in {
            Template.expand(Template("{?keys}"), vars) should equal("?keys=semi,%3B,dot,.,comma,%2C")
          }
          "{?keys*} => ?semi=%3B&dot=.&comma=%2C" in {
            Template.expand(Template("{?keys*}"), vars) should equal("?semi=%3B&dot=.&comma=%2C")
          }
        }
        "& Form-style query continuation" - {
          "{&var:3} => &var=val" in {
            Template.expand(Template("{&var:3}"), vars) should equal("&var=val")
          }
          "{&list} => &list=red,green,blue" in {
            Template.expand(Template("{&list}"), vars) should equal("&list=red,green,blue")
          }
          "{&list*} => &list=red&list=green&list=blue" in {
            Template.expand(Template("{&list*}"), vars) should equal("&list=red&list=green&list=blue")
          }
          "{&keys} => &keys=semi,%3B,dot,.,comma,%2C" in {
            Template.expand(Template("{&keys}"), vars) should equal("&keys=semi,%3B,dot,.,comma,%2C")
          }
          "{&keys*} => &semi=%3B&dot=.&comma=%2C" in {
            Template.expand(Template("{&keys*}"), vars) should equal("&semi=%3B&dot=.&comma=%2C")
          }
        }
      }
      "Expression Expansion" - {
        val vars = Seq(
          ListParam("count", Seq("one", "two", "three")),
          ListParam("dom", Seq("example", "com")),
          StringParam("dub", "me/too"),
          StringParam("hello", "Hello World!"),
          StringParam("half", "50%"),
          StringParam("var", "value"),
          StringParam("who", "fred"),
          StringParam("base", "http://example.com/home/"),
          StringParam("path", "/foo/bar"),
          ListParam("list", Seq("red", "green", "blue")),
          MapParam("keys", Map("semi" -> ";", "dot" -> ".", "comma" -> ",")),
          StringParam("v", "6"),
          StringParam("x", "1024"),
          StringParam("y", "768"),
          StringParam("empty", ""),
          MapParam("empty_keys", Map.empty),
          UndefParam("undef")
        )
        "Simple String Expansion: {var}" - {
          "{var} => value" in {
            Template.expand(Template("{var}"), vars) should equal("value")
          }
          "{hello} => Hello%20World%21" in {
            Template.expand(Template("{hello}"), vars) should equal("Hello%20World%21")
          }
          "{half} => 50%25" in {
            Template.expand(Template("{half}"), vars) should equal("50%25")
          }
          "O{empty}X => OX" in {
            Template.expand(Template("O{empty}X"), vars) should equal("OX")
          }
          "O{undef}X => OX" in {
            Template.expand(Template("O{undef}X"), vars) should equal("OX")
          }
          "{x,y} => 1024,768" in {
            Template.expand(Template("{x,y}"), vars) should equal("1024,768")
          }
          "{x,hello,y} => 1024,Hello%20World%21,768" in {
            Template.expand(Template("{x,hello,y}"), vars) should equal("1024,Hello%20World%21,768")
          }
          "?{x,empty} => ?1024," in {
            Template.expand(Template("?{x,empty}"), vars) should equal("?1024,")
          }
          "?{x,undef} => ?1024" in {
            Template.expand(Template("?{x,undef}"), vars) should equal("?1024")
          }
          "?{undef,y} => ?768" in {
            Template.expand(Template("?{undef,y}"), vars) should equal("?768")
          }
          "{var:3} => val" in {
            Template.expand(Template("{var:3}"), vars) should equal("val")
          }
          "{var:30} => value" in {
            Template.expand(Template("{var:30}"), vars) should equal("value")
          }
          "{list} => red,green,blue" in {
            Template.expand(Template("{list}"), vars) should equal("red,green,blue")
          }
          "{list*} => red,green,blue" in {
            Template.expand(Template("{list*}"), vars) should equal("red,green,blue")
          }
          "{keys} => semi,%3B,dot,.,comma,%2C" in {
            Template.expand(Template("{keys}"), vars) should equal("semi,%3B,dot,.,comma,%2C")
          }
          "{keys*} => semi=%3B,dot=.,comma=%2C" in {
            Template.expand(Template("{keys*}"), vars) should equal("semi=%3B,dot=.,comma=%2C")
          }
        }
        "Reserved Expansion: {+var}" - {
          "{+var} => value" in {
            Template.expand(Template("{+var}"), vars) should equal("value")
          }
          "{+hello} => Hello%20World!" in {
            Template.expand(Template("{+hello}"), vars) should equal("Hello%20World!")
          }
          "{+half} => 50%25" in {
            Template.expand(Template("{+half}"), vars) should equal("50%25")
          }
          "{base}index => http%3A%2F%2Fexample.com%2Fhome%2Findex" in {
            Template.expand(Template("{base}index"), vars) should equal("http%3A%2F%2Fexample.com%2Fhome%2Findex")
          }
          "{+base}index => http://example.com/home/index" in {
            Template.expand(Template("{+base}index"), vars) should equal("http://example.com/home/index")
          }
          "O{+empty}X => OX" in {
            Template.expand(Template("O{+empty}X"), vars) should equal("OX")
          }
          "O{+undef}X => OX" in {
            Template.expand(Template("O{+undef}X"), vars) should equal("OX")
          }
          "{+path}/here => /foo/bar/here" in {
            Template.expand(Template("{+path}/here"), vars) should equal("/foo/bar/here")
          }
          "here?ref={+path} => here?ref=/foo/bar" in {
            Template.expand(Template("here?ref={+path}"), vars) should equal("here?ref=/foo/bar")
          }
          "up{+path}{var}/here => up/foo/barvalue/here" in {
            Template.expand(Template("up{+path}{var}/here"), vars) should equal("up/foo/barvalue/here")
          }
          "{+x,hello,y} => 1024,Hello%20World!,768" in {
            Template.expand(Template("{+x,hello,y}"), vars) should equal("1024,Hello%20World!,768")
          }
          "{+path,x}/here => /foo/bar,1024/here" in {
            Template.expand(Template("{+path,x}/here"), vars) should equal("/foo/bar,1024/here")
          }
          "{+path:6}/here => /foo/b/here" in {
            Template.expand(Template("{+path:6}/here"), vars) should equal("/foo/b/here")
          }
          "{+list} => red,green,blue" in {
            Template.expand(Template("{+list}"), vars) should equal("red,green,blue")
          }
          "{+list*} => red,green,blue" in {
            Template.expand(Template("{+list*}"), vars) should equal("red,green,blue")
          }
          "{+keys} => semi,;,dot,.,comma,," in {
            Template.expand(Template("{+keys}"), vars) should equal("semi,;,dot,.,comma,,")
          }
          "{+keys*} => semi=;,dot=.,comma=," in {
            Template.expand(Template("{+keys*}"), vars) should equal("semi=;,dot=.,comma=,")
          }
        }
        "Fragment Expansion: {#var}" - {
          "{#var} => #value" in {
            Template.expand(Template("{#var}"), vars) should equal("#value")
          }
          "{#hello} => #Hello%20World!" in {
            Template.expand(Template("{#hello}"), vars) should equal("#Hello%20World!")
          }
          "{#half} => #50%25" in {
            Template.expand(Template("{#half}"), vars) should equal("#50%25")
          }
          "foo{#empty} => foo#" in {
            Template.expand(Template("foo{#empty}"), vars) should equal("foo#")
          }
          "foo{#undef} => foo" in {
            Template.expand(Template("foo{#undef}"), vars) should equal("foo")
          }
          "{#x,hello,y} => #1024,Hello%20World!,768" in {
            Template.expand(Template("{#x,hello,y}"), vars) should equal("#1024,Hello%20World!,768")
          }
          "{#path,x}/here => #/foo/bar,1024/here" in {
            Template.expand(Template("{#path,x}/here"), vars) should equal("#/foo/bar,1024/here")
          }
          "{#path:6}/here => #/foo/b/here" in {
            Template.expand(Template("{#path:6}/here"), vars) should equal("#/foo/b/here")
          }
          "{#list} => #red,green,blue" in {
            Template.expand(Template("{#list}"), vars) should equal("#red,green,blue")
          }
          "{#list*} => #red,green,blue" in {
            Template.expand(Template("{#list*}"), vars) should equal("#red,green,blue")
          }
          "{#keys} => #semi,;,dot,.,comma,," in {
            Template.expand(Template("{#keys}"), vars) should equal("#semi,;,dot,.,comma,,")
          }
          "{#keys*} => #semi=;,dot=.,comma=," in {
            Template.expand(Template("{#keys*}"), vars) should equal("#semi=;,dot=.,comma=,")
          }
        }
        "Label Expansion with Dot-Prefix: {.var}" - {
          "{.who} => .fred" in {
            Template.expand(Template("{.who}"), vars) should equal(".fred")
          }
          "{.who,who} => .fred.fred" in {
            Template.expand(Template("{.who,who}"), vars) should equal(".fred.fred")
          }
          "{.half,who} => .50%25.fred" in {
            Template.expand(Template("{.half,who}"), vars) should equal(".50%25.fred")
          }
          "www{.dom*} => www.example.com" in {
            Template.expand(Template("www{.dom*}"), vars) should equal("www.example.com")
          }
          "X{.var} => X.value" in {
            Template.expand(Template("X{.var}"), vars) should equal("X.value")
          }
          "X{.empty} => X." in {
            Template.expand(Template("X{.empty}"), vars) should equal("X.")
          }
          "X{.undef} => X" in {
            Template.expand(Template("X{.undef}"), vars) should equal("X")
          }
          "X{.var:3} => X.val" in {
            Template.expand(Template("X{.var:3}"), vars) should equal("X.val")
          }
          "X{.list} => X.red,green,blue" in {
            Template.expand(Template("X{.list}"), vars) should equal("X.red,green,blue")
          }
          "X{.list*} => X.red.green.blue" in {
            Template.expand(Template("X{.list*}"), vars) should equal("X.red.green.blue")
          }
          "X{.keys} => X.semi,%3B,dot,.,comma,%2C" in {
            Template.expand(Template("X{.keys}"), vars) should equal("X.semi,%3B,dot,.,comma,%2C")
          }
          "X{.keys*} => X.semi=%3B.dot=..comma=%2C" in {
            Template.expand(Template("X{.keys*}"), vars) should equal("X.semi=%3B.dot=..comma=%2C")
          }
          "X{.empty_keys} => X" in {
            Template.expand(Template("X{.empty_keys}"), vars) should equal("X")
          }
          "X{.empty_keys*} => X" in {
            Template.expand(Template("X{.empty_keys*}"), vars) should equal("X")
          }
        }
        "Path Segment Expansion: {/var}" - {
          "{/who} => /fred" in {
            Template.expand(Template("{/who}"), vars) should equal("/fred")
          }
          "{/who,who} => /fred/fred" in {
            Template.expand(Template("{/who,who}"), vars) should equal("/fred/fred")
          }
          "{/half,who} => /50%25/fred" in {
            Template.expand(Template("{/half,who}"), vars) should equal("/50%25/fred")
          }
          "{/who,dub} => /fred/me%2Ftoo" in {
            Template.expand(Template("{/who,dub}"), vars) should equal("/fred/me%2Ftoo")
          }
          "{/var} => /value" in {
            Template.expand(Template("{/var}"), vars) should equal("/value")
          }
          "{/var,empty} => /value/" in {
            Template.expand(Template("{/var,empty}"), vars) should equal("/value/")
          }
          "{/var,undef} => /value" in {
            Template.expand(Template("{/var,undef}"), vars) should equal("/value")
          }
          "{/var,x}/here => /value/1024/here" in {
            Template.expand(Template("{/var,x}/here"), vars) should equal("/value/1024/here")
          }
          "{/var:1,var} => /v/value" in {
            Template.expand(Template("{/var:1,var}"), vars) should equal("/v/value")
          }
          "{/list} => /red,green,blue" in {
            Template.expand(Template("{/list}"), vars) should equal("/red,green,blue")
          }
          "{/list*} => /red/green/blue" in {
            Template.expand(Template("{/list*}"), vars) should equal("/red/green/blue")
          }
          "{/list*,path:4} => /red/green/blue/%2Ffoo" in {
            Template.expand(Template("{/list*,path:4}"), vars) should equal("/red/green/blue/%2Ffoo")
          }
          "{/keys} => /semi,%3B,dot,.,comma,%2C" in {
            Template.expand(Template("{/keys}"), vars) should equal("/semi,%3B,dot,.,comma,%2C")
          }
          "{/keys*} => /semi=%3B/dot=./comma=%2C" in {
            Template.expand(Template("{/keys*}"), vars) should equal("/semi=%3B/dot=./comma=%2C")
          }
        }
        "Path-Style Parameter Expansion: {;var}" - {
          "{;who} => ;who=fred" in {
            Template.expand(Template("{;who}"), vars) should equal(";who=fred")
          }
          "{;half} => ;half=50%25" in {
            Template.expand(Template("{;half}"), vars) should equal(";half=50%25")
          }
          "{;empty} => ;empty" in {
            Template.expand(Template("{;empty}"), vars) should equal(";empty")
          }
          "{;v,empty,who} => ;v=6;empty;who=fred" in {
            Template.expand(Template("{;v,empty,who}"), vars) should equal(";v=6;empty;who=fred")
          }
          "{;v,bar,who} => ;v=6;who=fred" in {
            Template.expand(Template("{;v,bar,who}"), vars) should equal(";v=6;who=fred")
          }
          "{;x,y} => ;x=1024;y=768" in {
            Template.expand(Template("{;x,y}"), vars) should equal(";x=1024;y=768")
          }
          "{;x,y,empty} => ;x=1024;y=768;empty" in {
            Template.expand(Template("{;x,y,empty}"), vars) should equal(";x=1024;y=768;empty")
          }
          "{;x,y,undef} => ;x=1024;y=768" in {
            Template.expand(Template("{;x,y,undef}"), vars) should equal(";x=1024;y=768")
          }
          "{;hello:5} => ;hello=Hello" in {
            Template.expand(Template("{;hello:5}"), vars) should equal(";hello=Hello")
          }
          "{;list} => ;list=red,green,blue" in {
            Template.expand(Template("{;list}"), vars) should equal(";list=red,green,blue")
          }
          "{;list*} => ;list=red;list=green;list=blue" in {
            Template.expand(Template("{;list*}"), vars) should equal(";list=red;list=green;list=blue")
          }
          "{;keys} => ;keys=semi,%3B,dot,.,comma,%2C" in {
            Template.expand(Template("{;keys}"), vars) should equal(";keys=semi,%3B,dot,.,comma,%2C")
          }
          "{;keys*} => ;semi=%3B;dot=.;comma=%2C" in {
            Template.expand(Template("{;keys*}"), vars) should equal(";semi=%3B;dot=.;comma=%2C")
          }
        }
        "Form-Style Query Expansion: {?var}" - {
          "{?who} => ?who=fred" in {
            Template.expand(Template("{?who}"), vars) should equal("?who=fred")
          }
          "{?half} => ?half=50%25" in {
            Template.expand(Template("{?half}"), vars) should equal("?half=50%25")
          }
          "{?x,y} => ?x=1024&y=768" in {
            Template.expand(Template("{?x,y}"), vars) should equal("?x=1024&y=768")
          }
          "{?x,y,empty} => ?x=1024&y=768&empty=" in {
            Template.expand(Template("{?x,y,empty}"), vars) should equal("?x=1024&y=768&empty=")
          }
          "{?x,y,undef} => ?x=1024&y=768" in {
            Template.expand(Template("{?x,y,undef}"), vars) should equal("?x=1024&y=768")
          }
          "{?var:3} => ?var=val" in {
            Template.expand(Template("{?var:3}"), vars) should equal("?var=val")
          }
          "{?list} => ?list=red,green,blue" in {
            Template.expand(Template("{?list}"), vars) should equal("?list=red,green,blue")
          }
          "{?list*} => ?list=red&list=green&list=blue" in {
            Template.expand(Template("{?list*}"), vars) should equal("?list=red&list=green&list=blue")
          }
          "{?keys} => ?keys=semi,%3B,dot,.,comma,%2C" in {
            Template.expand(Template("{?keys}"), vars) should equal("?keys=semi,%3B,dot,.,comma,%2C")
          }
          "{?keys*} => ?semi=%3B&dot=.&comma=%2C" in {
            Template.expand(Template("{?keys*}"), vars) should equal("?semi=%3B&dot=.&comma=%2C")
          }
        }
        "Form-Style Query Continuation: {&var}" - {
          "{&who} => &who=fred" in {
            Template.expand(Template("{&who}"), vars) should equal("&who=fred")
          }
          "{&half} => &half=50%25" in {
            Template.expand(Template("{&half}"), vars) should equal("&half=50%25")
          }
          "?fixed=yes{&x} => ?fixed=yes&x=1024" in {
            Template.expand(Template("?fixed=yes{&x}"), vars) should equal("?fixed=yes&x=1024")
          }
          "{&x,y,empty} => &x=1024&y=768&empty=" in {
            Template.expand(Template("{&x,y,empty}"), vars) should equal("&x=1024&y=768&empty=")
          }
          "{&x,y,undef} => &x=1024&y=768" in {
            Template.expand(Template("{&x,y,undef}"), vars) should equal("&x=1024&y=768")
          }
          "{&var:3} => &var=val" in {
            Template.expand(Template("{&var:3}"), vars) should equal("&var=val")
          }
          "{&list} => &list=red,green,blue" in {
            Template.expand(Template("{&list}"), vars) should equal("&list=red,green,blue")
          }
          "{&list*} => &list=red&list=green&list=blue" in {
            Template.expand(Template("{&list*}"), vars) should equal("&list=red&list=green&list=blue")
          }
          "{&keys} => &keys=semi,%3B,dot,.,comma,%2C" in {
            Template.expand(Template("{&keys}"), vars) should equal("&keys=semi,%3B,dot,.,comma,%2C")
          }
          "{&keys*} => &semi=%3B&dot=.&comma=%2C" in {
            Template.expand(Template("{&keys*}"), vars) should equal("&semi=%3B&dot=.&comma=%2C")
          }
        }
      }
    }
  }
}

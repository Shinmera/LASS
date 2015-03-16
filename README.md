About LASS
----------
Writing CSS files comes with a lot of repetition and is generally much too verbose. With lispy syntax, shortcuts, and improvements, LASS aims to help you out in writing CSS quick and easy. LASS was largely inspired by [SASS](http://sass-lang.com/).

How To & Examples
-----------------
LASS supports two modes, one being directly in your lisp code, the other in pure LASS files. Adding LASS into your code is easy:

    (lass:compile-and-write
     '(div
       :background black))
    
    "div{
        background: black;
    }"

LASS works on the following simple principles: A list is a block. The first argument in the list is a selector. The body of the list makes up the properties and sub-blocks. A property is started with a keyword that is used as the property name. Following is a bunch of property arguments until a new keyword, list, or the end is reached. A list inside a block is, again, a block with the twist that the parent block's selector is prepended to the sub-block's selector.

    (lass:compile-and-write
     '(nav
       (ul
        :list-style none
        (li
         :margin 0 :padding 0
         :display inline-block)))))
    
    "nav ul{
        list-style: none;
    }
    
    nav ul li{
        margin: 0;
        padding: 0;
        display: inline-block;
    }"

Since LASS' `COMPILE-SHEET` simply takes a bunch of lists as its argument, you can use the backquote and comma to integrate variables from your lisp environment:

    (let ((color "#0088EE"))
      (lass:compile-and-write
       `(div
         :background ,color))))
    
    "div{
        background: #0088EE;
    }"

Alternatively however, and this is especially useful in pure LASS files, you can use the `LET` block to create LASS-specific bindings:

    (lass:compile-and-write
     '(:let ((color "#0088EE"))
       (div
        :background #(color))))
    
    "div{
        background: #0088EE;
    }"

LASS' selector mechanism is very flexible and allows for some complex logic to reduce duplication:

    (lass:compile-and-write
     '(article
       ((:or p blockquote)
        :margin 0 :padding 0
    
        (a
         :color black)
          
        ((:and a :hover)
         :color darkred))))
    
    "article p, article blockquote{
        margin: 0;
        padding: 0;
    }
    
    article p a, article blockquote a{
        color: black;
    }
    
    article p a:hover, article blockquote a:hover{
        color: darkred;
    }"

But it can go even further:

    (lass:compile-and-write
     '((:and
        (:or article section)
        (:= data-author (:or yukari ran chen))
        (:nth-child (:or 1 2 3)))
       :display none))
    
    "article[data-author=\"yukari\"]:nth-child(1),
     article[data-author=\"yukari\"]:nth-child(2),
     article[data-author=\"yukari\"]:nth-child(3),
     article[data-author=\"ran\"]:nth-child(1),
     article[data-author=\"ran\"]:nth-child(2),
     article[data-author=\"ran\"]:nth-child(3),
     article[data-author=\"chen\"]:nth-child(1),
     article[data-author=\"chen\"]:nth-child(2),
     article[data-author=\"chen\"]:nth-child(3),
     section[data-author=\"yukari\"]:nth-child(1),
     section[data-author=\"yukari\"]:nth-child(2),
     section[data-author=\"yukari\"]:nth-child(3),
     section[data-author=\"ran\"]:nth-child(1),
     section[data-author=\"ran\"]:nth-child(2),
     section[data-author=\"ran\"]:nth-child(3),
     section[data-author=\"chen\"]:nth-child(1),
     section[data-author=\"chen\"]:nth-child(2),
     section[data-author=\"chen\"]:nth-child(3){
        display: none;
    }"

Whoa nelly!

Some CSS properties are not fully specified yet and require browser-specific prefixes. LASS can help you with that, too:

    (lass:compile-and-write
     '(.fun
       :linear-gradient "deg(45)" black 0% darkgray 100%
       :transform rotate -45deg))
    
    ".fun{
        background: -moz-linear-gradient(deg(45), black 0%, darkgray 100%);
        background: -o-linear-gradient(deg(45), black 0%, darkgray 100%);
        background: -webkit-linear-gradient(deg(45), black 0%, darkgray 100%);
        background: -ms-linear-gradient(deg(45), black 0%, darkgray 100%);
        background: linear-gradient(deg(45), black 0%, darkgray 100%);
        -moz-transform: rotate(-45deg);
        -o-transform: rotate(-45deg);
        -webkit-transform: rotate(-45deg);
        -ms-transform: rotate(-45deg);
        transform: rotate(-45deg);
    }"

LASS also supports the various `@QUERY` operator blocks:

    (lass:compile-and-write
     '(:media "(max-width: 800px)"
       (div
        :margin 0)))
    
    "@media (max-width: 800px){
        div{
            margin: 0;
        }
    }"

By default LASS activates pretty-printing and inserts newlines and spaces where appropriate in order to make the result readable and easy to debug. However, you can also deactivate that and directly produce minified CSS:

    (let ((lass:*pretty* NIL))
      (lass:compile-and-write
       '(:media "(max-width: 800px)"
         (div
          :margin 0))))
    
    "@media (max-width: 800px){div{margin:0;}}"

As mentioned above you can write pure LASS files to compile down to a CSS file. To do that, simply use `GENERATE`:

![generate-example](http://shinmera.tymoon.eu/public/screenshot-2014.09.04-23:57:38.png)

Blocks
------
Each block in a LASS sheet consists of a list containing a selector followed by one or more properties or sub-blocks. 

    (selector [property | block]*)

Selectors
---------
The following list contains examples for the various uses of selectors.

* Any element  
  `*`
* An element with tag-name `e`  
  `e`
* An element with tag-name `e` or `f`  
  `(:or e f)`
* An `e` element with the `:link` pseudo-selector  
  `(:and e :link)`
* The first formatted line of an `e` element  
  `(:and e |::first-line|)` or `(:and e "::first-line")`
* An `e` element with a "warning" class  
  `e.warning`
* An `e` element with ID equal to `warning`  
  `|e#warning|` or `"e#warning"`
* An `e` element with a `foo` attribute  
  `e[foo]`
* An `e` element whose `foo` attribute value is exactly equal to `bar`  
  `(:and :a (:= foo "bar"))`
* An `e` element whose `foo` attribute value is a list of whitespace-separated values, one of which is exactly equal to `bar`  
  `(:and :a (:~= foo "bar"))`
* An `e` element whose `foo` attribute has a hyphen-separated list of values beginning (from the left) with `bar`  
  `(:and :a (:/= foo "bar"))`
* An `e` element whose `foo` attribute value begins exactly with the string `bar`  
  `(:and :a (:^= foo "bar"))`
* An `e` element whose `foo` attribute value ends exactly with the string `bar`  
  `(:and :a (:$= foo "bar"))`
* An `e` element whose `foo` attribute value contains the substring `bar`  
  `(:and :a (:*= foo "bar"))`
* An `e` element that matches the pseudo-selector `nth-child(2)`  
  `(e (:nth-child 2))`
* An `f` element preceded by an `e` element  
  `(e ~ f)`
* An `f` element immediately precede by an `e` element  
  `(e + f)`
* An `f` element which is a descendant of `e`  
  `(e f)`
* An `f` element which is a direct descendant of `e`  
  `(e > f)`


Selector Combinations
---------------------
As illustrated briefly above, LASS includes two combinators for selectors, `:and` and `:or`. These combinators are *combinatoric*, meaning that all possible combinations are explored. Consider the following selector:

    ((foo (:and a .title (:or :active :hover)) (:or span div)))

Enumerating all possible answers to this combination would result in the following list

    foo a.title:active span
    foo a.title:active div
    foo a.title:hover span
    foo a.title:hover div

The number of possible combinations can quickly explode in size the more options are available. This means that for complex relations and expressions, LASS can be extremely concise. Note that combinators are available at any position in a selector, this includes the arguments of a pseudo-selector like `:nth-child`.

Properties
----------
A property consists of a keyword symbol and a sequence of values. The values to a property are gathered up until either a non-value list or a new keyword is encountered. Originally it stopped as soon as a list was encountered, but this behaviour was changed and specially recognised lists are integrated to allow a more native look for certain values like colours, urls, and so on. Certain properties are specifically declared and will error if they are passed the wrong number or invalid kind of values. For most however, LASS will just blindly put things into the CSS file as you give them. It is up to you to make sure that the values are valid.

    :text-style underline
    :color (rgb 212 112 30)
    :background (url "/foo")
    :border 1px solid black

Certain properties currently still require vendor-specific declarations. LASS tries to do that automatically for you, but it also needs to know about these declarations and as such, they need to be manually added. Some of the more common ones are included in LASS by default, but if you encounter one that isn't, you are welcome to send a pull request (see Extending LASS on how to do it).

Sub-Blocks
----------
A block can contain other blocks. These sub-blocks are recursively flattened into the structure by simply prepending the selector of the parent block. Thus

    (foo (bar (baz) (bam)))

Is equivalent to

    (foo) ((foo bar)) ((foo bar baz)) ((foo bar bam))

Allowing this kind of nesting allows you to more closely mirror the structure present in your HTML file that you want to style. Combining this with the selector combinations, this system allows reducing code duplication a lot.

Special Blocks
--------------
In CSS3 there are special properties and blocks that are preceded by an `@` symbol. The most well-known examples therefore are probably `@include` and `@media`. LASS implements all of these special blocks by a keyword symbol equivalent selector. Therefore the above two would translate to the following in LASS.

    (:include (url "foo"))
    (:media "(max-width: 800px)"
     (foo))

Variables
---------
Often times it is useful to define variables that you can use within your style so that colours and fonts can quickly be exchanged. LASS allows you to do that too using the `:let` directive and by abusing the vector type. It is probably best illustrated using an example:

    (:let ((foo "#0088EE"))
      ((a:active) :color #(foo)))


Extending LASS
--------------
Pretty much every part of LASS is extensible through methods. Most useful will however probably be the `DEFINE-SPECIAL-PROPERTY`, `DEFINE-BROWSER-PROPERTY` and `DEFINE-SPECIAL-SELECTOR` helper-macros. Here's some examples from the `SPECIAL.LISP` file that defines some standard special handlers:

    (define-special-property font-family (&rest faces)
      (list (make-property "font-family" (format NIL "~{~a~^, ~}" (mapcar #'resolve faces)))))
    
    (define-browser-property linear-gradient (direction &rest colors)
      (:default (property)
        (make-property "background" (format NIL "~a(~a~{, ~a ~a~})"
                                             property (resolve direction) (mapcar #'resolve colors)))))

For more control, have a look at the various `COMPILE-*` generic functions.

Emacs Support
-------------
LASS includes a tiny elisp file, `lass.el`. Add LASS' directory to your emacs `LOAD-PATH` and `REQUIRE` lass.

    (add-to-list 'load-path "[path-to-lass-source-dir]/")
    (require 'lass)

Once you visit a `.lass` file, it will automatically start in the `LASS` major-mode, which is a derived-mode from `COMMON-LISP-MODE`. Whenever you save, it will automatically try to compile the lass file to its CSS equivalent. If slime is connected, it will try to quickload LASS and evaluate `GENERATE`. If slime is not connected, it instead executes a shell command. In order for that to work, the [`lass` binary](https://github.com/Shinmera/LASS/releases) must be in your path.

If your operating system is not directly supported with a binary, you can build it yourself using a build tool like [Buildapp](http://www.xach.com/lisp/buildapp/), the ASDF system `BINARY-LASS` and the entry-point `BINARY-LASS:CMD-WRAPPER`.

ASDF Integration
----------------
If you want to compile LASS files to CSS in your systems, you can now (v0.4+) do this via a `lass-file` component type, and `:defsystem-depends-on`-ing LASS.

    (asdf:defsystem my-system
      :defsystem-depends-on (:lass)
      :components ((:lass-file "test-file")))

You can also specify an `:output` argument to a `lass-file` to specify what the target css file should be.

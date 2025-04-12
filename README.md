# cl-yasboi: Yet Another Starter Boilerplate for Common Lisp 🔥

<p align="center">
  <img src="https://github.com/user-attachments/assets/93c5ed8b-c83c-418b-9222-8492b7835f84" />
</p>

`cl-yasboi` (pronounced *Yas boi!*) is a modern (*so 2025*), minimalist,
opinionated, and ***flamboyant*** Common Lisp starter project. It's designed to
be a source of inspiration and a starting point for your own projects.

This project is for both beginner and seasoned parentheses enthusiasts
evaluating modern setups.

## Features
- Uses `ASDF` for system(s) definition.
    - Defines both a main system and a test system.
    - Uses `ASDF`'s `:package-inferred-system` (the *modern* way) for less
      duplication of dependencies.
- Uses `Quicklisp` for downloading missing dependencies.
  - Sorry `Qlot`, maybe next year. `Quicklisp` is still too hot to ignore.
  - Uses external packages (the classics `alexandria` and `fiveam`) mostly for
    demonstration purposes.
- Tests with `fiveam`.
- Detailed installation and usage instructions (see below).
- Executable generation (with a predefined entrypoint).

## Quick Start
- Install [Steel Bank Common Lisp (SBCL)][1] (check your system's package
  manager).
- Install [Quicklisp][2] (See below for my custom instructions).
- Clone this repository into a folder where ASDF, and thereby Quicklisp, can
  find it (See below for instructions).
```bash
git clone https://github.com/sebastiancarlos/cl-yasboi.git
```
- Run it from the SBCL REPL:
```bash
sbcl
* (ql:quickload :cl-yasboi)
* (cl-yasboi:ayy-lmao) ; Run the main function
("ayy lmao")
```

[1]: https://www.sbcl.org/
[2]: https://www.quicklisp.org/beta/

## File Structure
```
cl-yasboi/
├── README.md       (<-- You are here!)
├── cl-yasboi.asd   (Project definition file)
├── cl-yasboi.lisp  (Main file)
├── lib.lisp        (Library file)
└── test.lisp       (Test file) 
```

## Rationale

Introductory Common Lisp texts rarely delve into the details of setting up a
project. This boilerplate aims to fill a bit of that gap, showcasing up-to-date
practices and tools.

## Usage

### Running the Project From the Lisp REPL
As Common Lisp is a REPL-heavy language, I'll describe first how to run the
project from the REPL.

I assume you have a Common Lisp implementation installed (If not, go to your
system's package manager and install one. I use Arch and SBCL, btw, so for me
it's `pacman -S sbcl`).

For any meaningful Common Lisp work, you need ASDF, which is Common Lisp's
*"package" "management"* solution.

Assuming your implementation has ASDF bundled in (which is the case for most
these days), you still need to obtain the project's external dependencies
(namely, `alexandria` and `fiveam`). Unless you have them already, or you have
a very power-user setup in place already, you need to install `Quicklisp`,
which is the tool which fetches external dependencies (Yes, the fetching tool
is separate from the main package management tool... I know).

Assuming you have a Unix system (*I know this*), just follow the standard
`Quicklisp` installation instructions, which I'll provide here:

```bash
# Download the Quicklisp bootstrap file
curl -O https://beta.quicklisp.org/quicklisp.lisp

# Load it onto your Lisp REPL
sbcl --load quicklisp.lisp

# Install it (You're now on the REPL, so the commands you type are those
# following the '*' prompt symbol.)
# Note: For this important step, I recommend installing Quicklisp on a path
#       which respects XDG, unless you want to see your home directory implode.
#       If no path is provided, it defaults to ~/quicklisp.
* (quicklisp-quickstart:install :path "~/.local/share/quicklisp")
```

There's one thing left to do, which is to add the `Quicklisp` initialization
function to your Lisp implementation's init file.

At this point (now assuming you're using SBCL) I'll implore you once more to
ensure that your SBCL's init file is *also* XDG-compliant. For that, I find
that the cleanest way is to add an alias to your bash (or zsh, fish) shell like
so:

```bash
# ~/.bashrc

alias sbcl="sbcl --load ~/.config/sbcl/init.lisp"
```

Or, if you're a *true chad* who squeezes every ounce of performance out of your
Unix system, you might prefer something like this:

```
alias sbcl="rlwrap sbcl --noinform --userinit ${XDG_CONFIG_HOME}/sbcl/init.lisp"
```

Now, add the following to your SBCL init file:

```lisp
; ~/.config/sbcl/init.lisp

; The following lines would have been added by the (ql:add-to-init-file)
; command, but Quicklisp refuses to accept non XDG-compliant Lisp init files,
; so this was copied manually:
#-quicklisp
(let ((quicklisp-init "~/.local/share/quicklisp/setup.lisp"))
  (load quicklisp-init)
  (format t "Loaded quicklisp ~a~%" quicklisp-init))
```

Congratulations. Almost done. ***I'm proud of you!***

Now, you need to ensure that `ASDF` (and by extension, `Quicklisp`) can find
this project. For that, you need to move it somewhere such as the following
default locations:

```bash
~/.local/share/common-lisp/source/
~/common-lisp/
```

I instead, a *scholar of the arts*, prefers to tell ASDF where to find my
projects one by one. I do it by (you guessed it) *respecting XDG*. You can do
so by creating a file such as the following (yes, the path *is* as long as a
`systemd` config):

```lisp
; ~/.config/common-lisp/source-registry.conf.d/local-asdf-projects.conf

(:directory "~/projects/cl-yasboi") ; add your projects one by one

; (:tree "~/lisp/") ; alternatively, designate a folder to be recursively
                    ; searched for ASDF projects.
```

That's it! you're ready to run your Common Lisp REPL and start the project:
```bash
# Start the REPL
sbcl
* (ql:quickload :cl-yasboi) ; Load the project (and fetch the dependencies, if
                            ; needed.
* (cl-yasboi:ayy-lmao) ; run its main function, "ayy-lmao"
("ayy lmao")
```

Success! 

### Running the Test Suite
Assuming you have SBCL and Quicklisp installed from the previous sections, just
do the following:

```bash
sbcl
* (ql:quickload "cl-yasboi/test")
* (asdf:test-system "cl-yasboi")
Running test suite CL-YASBOI-TEST 
... <tests> ...
T
```

The tests are passing! *Yas boi!*

### Generating Executables
The system definition file (`cl-yasboi.asd`) contains the configuration needed
for generating an executabls.

Particularly, the entry point for the executable is defined to be the Lisp form
`cl-yasboi:main`, which is a standard name for it.

You can generate an executable with:

```bash
sbcl
* (ql:quickload :cl-yasboi)
* (asdf:make :cl-yasboi)
```

Now, an executable `cl-yasboi` will be present in the project folder. You can
run it with:

```bash
./cl-yasboi
(ayy lmao)
```

Yet, it's about 30MB (it includes the whole SBCL runtime). But hey, an Electron
"hello world" app would still be over 160MB, so there's that! 😉

Also, it's not a cross-platform binary, which is outside of the scope of this
guide (*read: I don't know how to do it*). If you're interested, you can look
into ECL (Embedded Common Lisp) which promises way smaller binaries.

## Brief Description of Software Used

### Common Lisp
Despite common misconceptions, Common Lisp...
- is multi-paradigm (not functional-only),
- has a lot of baggage (not that minimalist), 
- has several interesting language features not frequently mentioned (the
  object system, the condition system, etc).

Note that Scheme and Clojure are the two other popular Lisps out there. Some
people claim that they are "Lisp, the good parts" to some degree, as they aim
to reduce complexity (more consistent interfaces, use of LISP-1 instead of
LISP-2, better package management, etc). However, Common Lisp wins for me in
terms of features, interactive development, performance, and JVM-avoidance.

In my opinion, Lisp is the answer to "What if the C preprocessor became a full
language, but operating on the AST instead of plain text?"

One downside of Common Lisp, which this boilerplate tries to mitigate, is the
package management ecosystem - which is lacking when compared to `npm`, `gem`,
`cargo-pants`, etc. I also feel that this issue is rarely mentioned early
enough on introductory material, which can catch beginners off guard.

#### External Resources
- [A Road to Common Lisp (article)](https://stevelosh.com/blog/2018/08/a-road-to-common-lisp)
  - Great article with a roadmap to learn Common Lisp.
- [Practical Common Lisp (book)](https://gigamonkeys.com/book/)
  - Likely the best book to learn Common Lisp, even if a bit dated (see article
    above for what to ignore).
- [The Common Lisp Cookbook](https://github.com/LispCookbook/cl-cookbook)
  - Great community resource.
- [Common Lisp Spec](https://cl-community-spec.github.io/pages/index.html)
  - Modern rendering of the spec. With dark mode! 😎
- [SBCL Manual](https://www.sbcl.org/manual/)
  - [Beyond the ANSI Standard" manual section](http://www.sbcl.org/manual/index.html#Beyond-the-ANSI-Standard)
    - This section explains the differences between SBCL and the ANSI standard.
- [Let Over Lambda](https://letoverlambda.com/)
  - Great resource for learning macros in depth.
- [Landscape of Lisp (2025)](https://churchofturing.github.io/landscapeoflisp.html)
  - A recent blog post on the state of Lisp.
- [djhasking's blog](https://blog.djhaskin.com/)
  - Nice blog about Lisp. With recent, well written articles.
- [Common Lisp community survey on ecosystem use (2024)](https://blog.djhaskin.com/blog/common-lisp-community-survey-2024-results/)
  - Great overview of the current tools used by the community.
- [Collections of quotes on why Lisp is great](https://lispers.org/) 
  - Evangelizing the masses 🙏
- For learning more about the Object System of Common Lisp, check the books:
  - "Object-Oriented Programming in Common Lisp: A Programmer's Guide to CLOS"
  - "The Art of the Metaobject Protocol"

### ASDF
ASDF is Common Lisp's "system definition facility." It's a *de facto* standard, as it comes included with every implementation.

Now, let's clarify some terminology. Common Lisp uses the word "package" for something a bit strange to people with modern sensibilities.
- A Common Lisp "package" is a collection of symbols. 
  - Think "namespaces" in other languages.
  - Also, a "package" provides some amount of encapsulation, as it can
    explicitly "export" some symbols only, and "import" some symbols from
    dependent packages.
  - A package is not  necesarily tied to a single file. They are created with
    `defpackage` functions. (See later for more).
- A "system" (as defined by ASDF) is a collection of source code and assets
  belonging to a Common Lisp project. 
  - Think "modules" or "packages" in other languages.
  - It's defined on the `.asd` file(s).
- A "project" is a collection of systems. 
  - For example, the `cl-yasboi` project contains the `cl-yasboi` system and
    the `cl-yasboi/test` system (a common arrangement).

Now, let me put on my SQL hat for a second and bring together these concepts 👷
- "project" is 1-to-many to "system". (but usually two or three systems per
  project).
- "system" is 1-to-many to "package".
- "package" is many-to-many to "files" (that is, no restrictions on how many
  files per package, or how many packages per file).
  - But notably, we're using the modern `package-inferred-system` ASDF system
    definition, which asks of us that we keep a 1-to-1 relationship between
    packages and files. This is a good practice, and closer to how other
    languages do it! Furthermore, it allows us to avoid duplicating the
    dependencies both at the system level and at the package level. Mind =
    blown 🤯

ASDF has two notable missing features:
- It doesn't fetch external dependencies.
- It doesn't verify the version of dependencies.
  - This is particularly funny because the ASDF manual suggesting that it
    supports a minimum version check by using the `:depends-on ((:version "foo"
    "3.1.2"))` syntax. But this is not actually being checked! *Ayy lmao!*

#### External Resources
- [Common Lisp Cookbook – Defining Systems](https://lispcookbook.github.io/cl-cookbook/systems.html)
- [Common Lisp Cookbook – Getting started](https://lispcookbook.github.io/cl-cookbook/getting-started.html)
- [ASDF Manual](https://asdf.common-lisp.dev/asdf.pdf)
- [ASDF "Best practices" document](https://gitlab.common-lisp.net/asdf/asdf/blob/master/doc/best_practices.md)
  - Slightly outdated, 2017.
- [UIOP Manual](https://asdf.common-lisp.dev/uiop.html)

### Quicklisp
Quicklisp is the tool which fetches external dependencies for Common Lisp. It's
tightly integrated with ASDF, and supports many Common Lisp implementations.

There are however two significant downsides:
- It uses HTTP for fetching, which has some security implications.
  - If this is a particular concern of yours, then you might need to evaluate
    Quicklisp alternatives, use one of the Quicklisp forks which address this,
    or build your custom workflow for fetching dependencies (using cutting-edge
    technology like `curl`!)
- It, like ASDF, doesn't verify the version of dependencies.

That's right, Quicklisp always fetches the *latest* version available of any
dependency. 

Basically, Quicklisp keeps a list of all available software (a "dist"). A dist
is like a snapshot, which is is updated monthly (although in practice, the
latest update as of 2025-04 was 2024-10, accompanied by the message "Sorry this
update took so long. My goal is to resume monthly releases", which didn't
happen.)

At this point I want to mention that I'm in no way trying to attack the author
and maintainer of Quicklisp, who has single-handedly kept this ship running for
years to great benefit for the community! To some degree, the lack of frequent
dist updates and strict version pinning is more a reflection of the ecosystem's
size and the excellent backwards-compatibility of Common Lisp software; One can
expect that as the ecosystem requirements grow, the required capital would
emerge to improve the tooling accordingly.

Having said that, you might want to consider using "Ultralisp", which is an
alternative Quicklisp "dist" which is updated very frequently.

#### External Resources
- [Quicklisp Homepage](https://www.quicklisp.org)
- [Quicklisp Source Code](https://github.com/quicklisp/quicklisp-client)
- [Ultralisp Homepage](https://ultralisp.org/)

### FiveAM
FiveAM is a mature testing framework for Common Lisp.

It has has 3 levels of abstraction: `check` (like "assertion" in other
langauges), `test` and `suite`. It works as you would expect.

The most common check is `is`, which expects and expression to return `T`
(true).

#### External Resources
- [Common Lisp Cookbook's FiveAM Guide](https://lispcookbook.github.io/cl-cookbook/testing.html)
- [FiveAM docs](https://fiveam.common-lisp.dev/docs/index.html)
  - Note that the FiveAM docs are written in a quasi-literate-programming
    style, which makes it virtually unreadable for the casual user. So, I
    recommend the Cookbook's guide above.

### Qlot (Honorable Mention)
For those requiring strict version control, Qlot is currently the most popular alternative to Quicklisp.


However, at this time, at a personal quick glance, I don't find that Qlot
provides a sufficiently smooth experience, particularly considering Quicklisp's
overwhelming "default status," and the fact that Qlot basically wraps
Quicklisp. Therefore, my actual recommendation is to keep using Quicklisp for
the time being, particularly taking it as a default when sharing your projects.

I do expect that if an acceptable solution to the versioning problem is found,
it would take the form of a patch or a fork to Quicklisp, rather than a
solution which wraps Quicklisp. But that's just my opinion.

#### External Resources
- [Qlot Homepage](https://qlot.tech/)

## Contributing

If you want to contribute to this project, please open an issue or a pull
request!

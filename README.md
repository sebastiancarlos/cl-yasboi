# cl-yasboi: Yet Another Starter Boilerplate for Common Lisp 🔥

<p align="center">
  <img src="https://github.com/user-attachments/assets/93c5ed8b-c83c-418b-9222-8492b7835f84" />
</p>

`cl-yasboi` (pronounced *Yas boi!*) is a modern (*so 2025*), minimalist,
opinionated, UNIX-based, and ***flamboyant*** Common Lisp starter project. It's
designed to be a source of inspiration and a starting point for your own
projects.

This project is for both beginner and seasoned parentheses enthusiasts
evaluating modern setups.

![cl-yasboi](https://github.com/sebastiancarlos/cl-yasboi/actions/workflows/ci.yml/badge.svg)
![License](https://img.shields.io/badge/License-MIT-green)

## Quick Start

```bash
$ git clone https://github.com/sebastiancarlos/cl-yasboi.git && cd cl-yasboi
$ make build
$ ./cl-yasboi
(ayy lmao)
```

## Table of Contents

- [Features](#features)
- [File Structure](#file-structure)
- [Rationale](#rationale)
- [Running the Project From the Lisp REPL](#running-the-project-from-the-lisp-repl)
  - [Setting up Quicklisp and ASDF](#setting-up-quicklisp-and-asdf)
  - [Configuring ASDF to Find Your Project](#configuring-asdf-to-find-your-project)
- [Running the Test Suite](#running-the-test-suite)
- [Generating Executables](#generating-executables)
- [Automated Build and Installation (Makefile)](#automated-build-and-installation-makefile)
- [Automated Testing (Makefile)](#automated-testing-makefile)
- [Continuous Integration (CI)](#continuous-integration-ci)
- [Setting Up an Interactive Editor Workflow](#setting-up-an-interactive-editor-workflow)
  - [SLIME for Emacs](#slime-for-emacs)
  - [Slimv for Vim/Neovim](#slimv-for-vimneovim)
  - [Interactive Development Example With Slimv](#interactive-development-example-with-slimv)
- [Brief Description of Software Used](#brief-description-of-software-used)
  - [Common Lisp](#common-lisp)
  - [ASDF](#asdf)
  - [Quicklisp](#quicklisp)
  - [FiveAM](#fiveam)
  - [Qlot (Honorable Mention)](#qlot-honorable-mention)
- [Contributing](#contributing)

## Features

- Uses `ASDF` to define systems.
    - Defines both a main system and a test system.
    - Uses `ASDF`'s `:package-inferred-system` (the *modern* way) for less
      duplication of dependencies.
- Uses `Quicklisp` for downloading missing dependencies.
  - Sorry `Qlot`, maybe next year. `Quicklisp` is still too hot to ignore.
  - Uses external packages (the classics `alexandria` and `fiveam`) mostly for
    demonstration purposes.
- Allows end-users to easily create an executable with `make`.
- Tests with `fiveam`.
- Detailed installation and usage instructions (see below).
- Executable generation (with a predefined entrypoint).

## File Structure

```
cl-yasboi/
├── README.md            (<-- You are here!)
├── cl-yasboi.asd        (Project definition file)
├── cl-yasboi.lisp       (Main file)
├── lib.lisp             (Library file)
├── test.lisp            (Test file)
├── Makefile             (Build and install commands)
├── .internal-scripts/   (Scripts used by Makefile)
└── .github/workflows/   (Continuous Integration)
```

## Rationale

Introductory Common Lisp texts rarely delve into the details of setting up a
project. This boilerplate aims to fill that gap, showcasing up-to-date
practices and tools.

## Running the Project From the Lisp REPL

As Common Lisp is a REPL-heavy language, I'll describe first how to run the
project from the REPL.

**Note:** If you already have a working Lisp + Quicklisp + ASDF setup
configured (***my man!***), you can skip to the end of this section.

I assume you have a Common Lisp implementation installed (If not, check your
system's package manager and install one. I use Arch (*btw*) and SBCL, so for
me it's `pacman -S sbcl`).

### Setting up Quicklisp and ASDF

For any meaningful Common Lisp work, you need ASDF, which is Common Lisp's
*"package" "management"* solution.

Assuming your implementation has ASDF bundled in (which is the case for most
these days), you still need to get this project's external dependencies
(namely, `alexandria` and `fiveam`). Unless you have them already, or you have
a very *power-user* setup in place already, you need to install
[Quicklisp](https://www.quicklisp.org/beta/), which is the tool that fetches
external dependencies (Yes, the fetching tool is separate from the main package
management tool... *I know*).

Assuming you have a Unix system (*I know this*), just follow the standard
Quicklisp installation instructions, which I'll provide here with a
modification for better XDG support:

```bash
# Download the Quicklisp bootstrap file
# (Don't mind the "beta" in the URL, that's the official and current URL)
curl -O https://beta.quicklisp.org/quicklisp.lisp

# Load it onto your Lisp REPL
sbcl --load quicklisp.lisp

# Install it (You're now on the REPL, so the commands you type are those
# following the '*' prompt symbol)..
# Note: For this important step, I recommend installing Quicklisp on a path
#       which respects XDG, unless you want to see your home directory implode.
#       If no path is provided, it defaults to ~/quicklisp.
* (quicklisp-quickstart:install :path "~/.local/share/quicklisp")
```

There's one thing left to do: Adding the Quicklisp initialization function to
your Lisp implementation's init file.

At this point (assuming you're using SBCL) I'll implore you once more to ensure
your SBCL's init file is *also* XDG-compliant. For that, I find the cleanest
way is to add an alias to your bash (or zsh, fish) shell:

```bash
# ~/.bashrc

alias sbcl="sbcl --userinit ~/.config/sbcl/init.lisp"
```

Or, if you're a *true chad* who squeezes every ounce of performance out of your
Unix system, you might prefer something like this:

```bash
alias sbcl="rlwrap \
  --history-filename ${XDG_DATA_HOME}/sbcl_history \
  sbcl --noinform \
    --userinit ${XDG_CONFIG_HOME}/sbcl/init.lisp"
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

### Configuring ASDF to Find Your Project

Now, you need to ensure that ASDF (and by extension, Quicklisp) can find
this project. For that, you need to move it somewhere like the following
default locations:

```bash
~/.local/share/common-lisp/source/
~/common-lisp/
```

I instead, a *scholar of the arts*, prefer to tell ASDF where to find my
projects one by one. I do it by (you guessed it) *respecting XDG*. You can do
so by creating a file like the following (yes, the path **is** as long as a
`systemd` config):

```lisp
; ~/.config/common-lisp/source-registry.conf.d/local-asdf-projects.conf

(:directory "~/projects/cl-yasboi") ; Add your projects one by one.

; (:tree "~/lisp/") ; Alternatively, designate a folder to be recursively
                    ; searched for ASDF projects.
```

That's it! you're ready to run your Common Lisp REPL and start the project:
```bash
# Start the REPL
sbcl
* (ql:quickload :cl-yasboi) ; Load the project (and fetch the dependencies, if
                            ; needed)
* (cl-yasboi:ayy-lmao) ; Run its main function, "ayy-lmao"
("ayy lmao")
```

***Success!***

## Running the Test Suite

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

## Generating Executables

The system definition file (`cl-yasboi.asd`) contains the configuration needed
for generating an executable.

Particularly, the entry point for the executable is defined to be the Lisp form
`cl-yasboi:main`, which is a standard name for it.

You can generate an executable with:

```bash
sbcl
* (ql:quickload :cl-yasboi)
* (asdf:make :cl-yasboi)
```

Now, the executable `cl-yasboi` will be present in the project folder. You can
run it with:

```bash
./cl-yasboi
(ayy lmao)
```

Yes, it's about 30MB (it includes the whole SBCL runtime). But hey, an Electron
"hello world" app would still be over 160MB, so there's that! 😉

Also, it's not a cross-platform binary, which is outside of the scope of this
guide (*read: I don't know how to do it*). If you're interested, you can look
into [ECL](https://ecl.common-lisp.dev/) (Embeddable Common-Lisp) which
promises way smaller binaries.

## Automated Build and Installation (Makefile)

While the REPL-based approach to make an executable is standard for Common Lisp
development, we need to provide something more convenient for the final user.

So, this project includes a `Makefile` (the universal UNIX build interface)
with the following commands:
- `make build` generates the executable (Only requires `sbcl` and `curl`).
- `make install` puts the generated executable in a standard `PATH` location
  (and generates the executable if not done already)
- `make` is an alias for `make install`.

We generate the executable by installing and using a temporary Quicklisp
environment in the project folder, which is cleaned up afterwards. We do this
because we can't assume how the user has ASDF and Quicklisp setup, if at all. 

The full logic can be found on the `.internal-scripts/` folder, which is called
by the `Makefile`. But here's the gist:

1.  Download the `quicklisp.lisp` bootstrap file using `curl`.
2.  Install Quicklisp into a local directory within the project: `./.ql-tmp/`.
3.  Use that Quicklisp instance to download the project's dependencies, compile
    the project, and build the final executable.
4.  Automatically cleans up afterwards (whether success or error).
5.  `make install` copies the executable to the standard PATH location
    `/usr/local/bin` (configurable), making it accessible system-wide.

## Automated Testing (Makefile)

Similar to the automated build above, you can *also* run the test suite with
`make test`.

At this point, I would like to clarify a design decision. Our `Makefile`
orchestration consists of:
- A minimal `Makefile` for standard interface,
- a series of `.bash` scripts for system logic, abstracting common operations,
  and
- a series of `.lisp` files for the actual Lisp work.

An alternative, which was tested and ultimately discarded, was to keep the
minimal `Makefile` but move the rest of the logic to a single `.lisp` file.
This had the advantages of language cohesion and ease of modification.

However, it increased the complexity for several reasons:
- To generate a build, the `lisp` process must exit, which removes the chance
  of any subsequent cleanup unless starting a nested child Lisp process (the
  sort of task which is more straightforward from Bash).
- Similarly, it's my understanding that Bash is better suited to handle
  platform-specific concerns like that succinctly.

For those worried about the difficulty of modifying the automated orchestration
logic due to the current setup, I would point out that:
- I believe the current state is suitable for most kinds of Lisp projects. Even
  if you were developing a library and have little need for an executable, I
  would argue that most libraries would benefit from having a secondary,
  minimal executable for discoverability.
- Even if you do want to change, add, or remove the orchestration steps, you
  won't have much inconvenience despite the extra files and plurality of
  languages.

## Continuous Integration (CI)

This project contains a "Github Actions" workflow to run the build and tests on
the default Ubuntu runner. It runs in 20 seconds (or 10 on a good day!), which
is quite alright, thanks to some `apt` caching and related trickery.

Ideally, I would have a generic CI solution working across CI providers, CPU
architectures, macOS, and Linux. However, that would require a way to get
up-to-date SBCL binaries for all those targets. *Unfortunately*:

- The [SBCL website](https://www.sbcl.org/platform-table.html) offers
  pre-compiled binaries of its *latest* version only for x86 Linux. Other
  platforms are supported, but official binaries are old or *non-existent*.
- Even if I decided to build SBCL from source for CI, that would *still*
  require a previously available Lisp binary on the system (*yo dawg*)! And
  even getting *any old random* Lisp binary on a system can be tricky; This is
  a known problem recognized by others such as ASDF's SBCL plugin (No, not
  *our* ASDF, but the [language-agnostic version
  manager](https://github.com/smashedtoatoms/asdf-sbcl)) on this ***tragically
  hilarious*** quote:

>  "SBCL is compiled using itself, or any other Common Lisp. Since MacOS
>  Ventura, the old builds don't run anymore due to `mmap` errors. To deal with
>  that, I now use ECL for mac builds which is an embeddable Common Lisp that
>  is widely available, though quite slow. [...] You're honestly probably
>  better served loading SBCL from your local package manager."

In short, while it's possible to have a generic and slightly faster CI
solution, that would fall out of the scope of this minimal boilerplate without
introducing more dependencies (maybe Docker or Roswell) or *byzantine*
compilation steps. Best I can do is a cached `apt-get install sbcl`.

I hope for an improvement to SBCL's binary distribution situation in the
future, so that this project can escape from Github CI's vendor lock-in, as I'm
sure any bare-bones Hetzner CI would run circles around it. Hey, even a Lenovo
laptop on your garage would!

## Setting Up an Interactive Editor Workflow

A cornerstone of productive Common Lisp development is the interactive editor
workflow. Instead of the typical edit-compile-run cycle found in many
languages, Lisp development often involves connecting your editor directly to a
running Lisp process (an "image"). 

This allows you to evaluate code snippets directly from your editor, inspect
the state of your program, and debug interactively—significantly speeding up
development.

Let's put it like this: Most programming languages have editable source code
and REPLs. Common Lisp is likely the best one to mix **both** so effectively
(with the possible exception of Smalltalk). 

Modern languages *cannot even get close* to this power level. It's true that
interactive editing has seen recent focus, but the fact is that Lisp was
designed from the start with this in mind, so it has facilities that will
likely keep it at the front of the pack.

Having said that, this workflow is not as strictly necessary as some zealots
would make you believe. If you're new to Common Lisp, you're free to ignore
this for now, or just get a light setup going. You can deep-dive into this
later, when you feel more comfortable with the language. 

One final warning though: For walking down this path, Emacs or Vim provide the
best experience. If you don't know them, I do recommend learning one of them -
I'll be waiting here for you. Having said that, I heard that [VSCode +
Alive](https://lispcookbook.github.io/cl-cookbook/vscode-alive.html) is a
reasonable alternative.

### SLIME for Emacs

The traditional and most feature-rich environment for this is [SLIME (Superior
Lisp Interaction Mode for Emacs)](https://common-lisp.net/project/slime/) for
the Emacs editor. SLIME communicates with a backend server called **SWANK**
running within your Lisp process.

Even if you don't use Emacs, the SWANK server is the basis for integration in
Vim too.

I'll focus the rest of this quick guide on Vim usage as a Vim user myself
(also, any Emacs person reading this already has everything set up since the
80s).

### Slimv for Vim/Neovim

The best Vim/Neovim plugin is [Slimv](https://github.com/kovisoft/slimv). To
set it up, go check the [Official
Tutorial](https://kovisoft.github.io/slimv-tutorial/tutorial.html) (Be warned
that the tutorial mentions `asdf-install`, which has been replaced years ago by
Quicklisp—everything else still stands).

Here's my configuration for `slimv`:

```vim
" Custom command to start SWANK for slimv
" NOTE: This assumes "neovim", and "plugged" as the plugin manager.
let g:slimv_swank_cmd =
    \ '!tmux new -d -s my-swank "sbcl ' .
    \ '--userinit ${XDG_CONFIG_HOME}/sbcl/init.lisp ' .
    \ '--load ${XDG_DATA_HOME}/nvim/plugged/slimv/slime/start-swank.lisp"'

" Open the REPL on a vertical split
let g:slimv_repl_split = 4

" make sure paredit use the leader "," for all commands
let g:paredit_leader = ","

" don't put compiled files in your current directory
let g:slimv_fasl_directory = "/tmp/"

" rainbow parentheses for lisp (I disable it because it looks *terrible*)
"let g:lisp_rainbow = 1
```

One quirk of this setup is that the command to start the SWANK server requires
a terminal multiplexer (`tmux` in this case). This is required because `sbcl`
has ***complete disregard*** for basic UNIX shell's job control features
(basically, it rejects anything but full ownership of its terminal, so we make
it believe that it's the *only process in the world* by using `tmux`).

Keep in mind that both SLIME and Slimv incorporate **paredit**, a great tool
for editing Lisp code (It's covered in the Slimv tutorial above). Of course,
these editors come with great generic code-editing features; it's up to you to
find your balance between paredit and the generic features.

#### More resources:
- [Lisp on Vim](https://susam.net/lisp-in-vim.htm)
- Don't miss the online help on vim, which is the only source for some features
  (`:help slimv`).

### Interactive Development Example With Slimv

Here's a quick demo of how to edit this very project using Slimv (It should be
easily adapable to SLIME too).

- Open the `lib.lisp` file. 
- Press `,c` to connect to the Lisp process.
- Type the following on the in-editor REPL to load the entire project:
  ```lisp
  CL-USER> (ql:quickload :cl-yasboi)
  ```
- Now, modify the `lmao` function to print `"lmaoooo"` instead of `"lmao"`.
- Evaluate it with `,e`.
- Now, back at the REPL, run the main project's function:
  ```lisp
  CL-USER> (cl-yasboi:ayy-lmao)
  "ayy lmaoooo"
  ```

## Brief Description of Software Used

### Common Lisp

Despite common misconceptions, Common Lisp...
- is multi-paradigm (not functional-only),
- has a lot of baggage and batteries included (not that minimalist),
- has several interesting, almost unique language features not frequently
  mentioned (the object system, the condition system, etc).

Note that [Scheme](https://www.gnu.org/software/guile/) and
[Clojure](https://clojure.org/) are the two other popular Lisps out there. Some
people claim that they are *"Lisp, the good parts"*, as they aim to reduce
complexity (more consistent interfaces, use of LISP-1 instead of LISP-2, better
package management, etc). However, Common Lisp wins for me in terms of
features, interactive development, performance, and JVM-avoidance.

In my opinion, Lisp is the answer to "What if the C preprocessor became a full
language, but operating on the AST instead of plain text?"

One downside of Common Lisp, which this boilerplate tries to mitigate, is the
package management tooling - which is lacking compared to `npm`, `gem`,
***`cargo-pants`***, etc. I also feel that this issue is rarely mentioned early
enough in introductory material, which can catch beginners off guard.

#### External Resources

- [A Road to Common Lisp (article)](https://stevelosh.com/blog/2018/08/a-road-to-common-lisp)
  - Great article with a roadmap to learn Common Lisp.
- [Practical Common Lisp (book)](https://gigamonkeys.com/book/)
  - Likely the best book to learn Common Lisp, even if a bit dated (see article
    above for what to ignore).
- [The Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook/)
  ([Source code](https://github.com/LispCookbook/cl-cookbook))
  - Great community resource.
- [Common Lisp Spec](https://cl-community-spec.github.io/pages/index.html)
  - Modern rendering of the spec. With dark mode! 😎
- [SBCL Manual](https://www.sbcl.org/manual/)
  - ["Beyond the ANSI Standard" section](http://www.sbcl.org/manual/index.html#Beyond-the-ANSI-Standard)
    - This section explains the differences between SBCL and the ANSI standard.
- [Let Over Lambda](https://letoverlambda.com/)
  - Great resource for learning macros (including reader macros) in depth.
- [Landscape of Lisp (2025)](https://churchofturing.github.io/landscapeoflisp.html)
  - A recent blog post on the state of Lisp.
- [djhasking's blog](https://blog.djhaskin.com/)
  - Nice blog about Lisp. With recent, well written articles.
- [Common Lisp community survey on ecosystem use (2024)](https://blog.djhaskin.com/blog/common-lisp-community-survey-2024-results/)
  - Great overview of the current tools used by the community.
- [Collections of quotes on why Lisp is great](https://lispers.org/) 
  - Evangelizing the masses 🙏
- [Lisp subreddit](https://www.reddit.com/r/lisp/)
- For learning more about the Object System of Common Lisp, check the
  **books**:
  - *"Object-Oriented Programming in Common Lisp: A Programmer's Guide to
    CLOS"*
  - *"The Art of the Metaobject Protocol"*

### ASDF

ASDF is Common Lisp's "system definition facility." It's the *de facto*
standard, as it comes included with every implementation.

Now, let's clarify some terminology. Common Lisp uses the word "package" for
something a bit strange to people used to *modern* languages.
- A Common Lisp **"package"** is a collection of symbols. 
  - Think "namespaces" in other languages.
  - Also, a "package" provides some amount of encapsulation, as it can
    explicitly "export" symbols from other packages, and "import" symbols from
    dependent packages.
  - A package is not  necessarily tied to a single file. They are created with
    `defpackage` functions. (See later for more).
- A **"system"** (as defined by ASDF) is a collection of source code and assets
  belonging to a Common Lisp project. 
  - Think "modules" or "packages" in other languages.
  - A system is defined on an `.asd` file. Actually, a common practice is to
    define all systems for your project on the same `.asd` file.
- A **"project"** is a collection of systems. 
  - For example, the `cl-yasboi` project contains the `cl-yasboi` system and
    the `cl-yasboi/test` system (a common arrangement).

Now, let me put on my *SQL hat* for a second and bring together the
***cardinality*** these concepts 👷
- **"project"** is **1-to-many** to **"system"**. (but usually just two or
  three systems per project).
- **"system"** is **1-to-many** to **"package"**.
- **"package"** is **many-to-many** to **"files"** (that is, no restrictions on
  how many files per package, or how many packages per file).
  - But notably, we're using the modern `:package-inferred-system` ASDF system
    definition, which asks of us that we keep a **1-to-1** relationship between
    packages and files. This is a good practice, and closer to how other
    languages do it! Furthermore, it allows us to avoid duplicating the
    dependencies both at the system level and at the package level. *Mind =
    blown* 🤯

Despite using `:package-inferred-system` and thereby not needing to list any
dependencies besides your main systems on the `.asd` file, I still recommend
listing your *external* dependencies on the `.asd` file. That way, people can
quickly see what you project depends on.

ASDF has two notable missing features:
- It doesn't download external dependencies.
- It doesn't verify the version of dependencies.
  - This is particularly funny because the ASDF manual
    [suggests](https://asdf.common-lisp.dev/asdf.html#Version-identifier) that
    it supports a minimum version check by using the `:depends-on ((:version
    "foo" "3.1.2"))` syntax. But this is not actually being checked! *Ayy
    lmao!*

#### External Resources

- [Common Lisp Cookbook – Defining Systems](https://lispcookbook.github.io/cl-cookbook/systems.html)
- [Common Lisp Cookbook – Getting started](https://lispcookbook.github.io/cl-cookbook/getting-started.html)
- [Arch Wiki - Common Lisp](https://wiki.archlinux.org/title/Common_Lisp)
  - Great general resource, with detailed system management information.
- [ASDF Manual](https://asdf.common-lisp.dev/asdf.pdf)
- [ASDF "Best practices" document](https://gitlab.common-lisp.net/asdf/asdf/blob/master/doc/best_practices.md)
  - Slightly outdated, 2017.
- [UIOP Manual](https://asdf.common-lisp.dev/uiop.html)

### Quicklisp

Quicklisp is the tool which downloads external dependencies for Common Lisp.
It's tightly integrated with ASDF, and supports many Common Lisp
implementations.

There are however two significant downsides:
- It uses HTTP for fetching, which has some security implications.
  - If this is a particular concern, you might need to evaluate Quicklisp
    alternatives, use one of the Quicklisp forks which address this, or build
    your custom workflow for fetching dependencies (using *cutting-edge
    technologies* like `curl`!)
- It, like ASDF, doesn't verify the version of dependencies.

That's right, Quicklisp always fetches the *latest* version available of any
dependency. 

Basically, Quicklisp keeps a list of all available software (a **"dist"**). A
dist is like a snapshot, which is is updated monthly (although in practice, the
latest update as of 2025-04 was 2024-10, accompanied by the
[message](http://blog.quicklisp.org/2024/10/october-2024-quicklisp-dist-update-now.html)
*"Sorry this update took so long. My goal is to resume monthly releases"*,
which didn't happen).

At this point I want to mention that I'm in no way trying to attack the author
and maintainer of Quicklisp, who has single-handedly kept this ship running for
years to great benefit of the community! To some degree, the lack of frequent
updates and strict versioning is more a reflection of the ecosystem's size and
the excellent backwards-compatibility of Common Lisp software; One can expect
that as the ecosystem grows, the necessary resources will emerge to improve the
tooling accordingly.

Having said that, you might want to consider using
[Ultralisp](https://ultralisp.org/), an alternative Quicklisp "dist" which is
updated very frequently.

#### External Resources

- [Quicklisp Homepage](https://www.quicklisp.org)
- [Quicklisp Source Code](https://github.com/quicklisp/quicklisp-client)
- [Ultralisp Homepage](https://ultralisp.org/)

### FiveAM

FiveAM is a mature testing framework for Common Lisp.

It has 3 levels of abstraction: `check` (like "assertion" in other langauges),
`test` and `suite`. It works as you would expect.

The most common check is `is`, which expects and expression to return `T`
(true).

#### External Resources

- [Common Lisp Cookbook's FiveAM Guide](https://lispcookbook.github.io/cl-cookbook/testing.html)
- [FiveAM docs](https://fiveam.common-lisp.dev/docs/index.html)
  - Note that the FiveAM docs are written in a quasi literate-programming
    style, which makes it virtually unreadable for the casual user. So, I
    recommend the Cookbook's guide above.

### Qlot (Honorable Mention)

For those wanting strict version control, Qlot is currently the most popular
alternative to Quicklisp.

However, at this time, at a personal quick glance, I don't find that Qlot
provides a sufficiently smooth experience, particularly considering Quicklisp's
overwhelming "default status," and the fact that Qlot basically wraps
Quicklisp. Therefore, my actual recommendation is to keep using Quicklisp for
the time being, particularly taking it as a default when sharing your projects.

I do expect that if an acceptable solution to the versioning problem is found,
it will take the form of a patch or a fork to Quicklisp, rather than a solution
which wraps Quicklisp. But that's just my opinion.

Also, check [Vend](https://github.com/fosskers/vend) for donwloading
dependencies directly into your project repository.

#### External Resources

- [Qlot Homepage](https://qlot.tech/)

## Contributing

Contributions are welcome! If you want to contribute to this project, please
open an issue or a pull request!

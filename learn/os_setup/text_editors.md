---
layout: book
title: Text Editors
permalink: /learn/os_setup/text_editors
---

After you have installed your compiler, you will need a text editor to write your code. Any text editor can serve this purpose, even the built-in Notepad on Windows. However, there are specialized editors for programming languages. These editors come with many useful features like auto-complete, syntax-highlighting, auto-indentation, brace-matching, and many more, with few of them pre-installed and the majority in form of external plug-ins. This means that by default these features are not installed in the editor, but it's up to you to search for them through a package manager and install and configure them manually. 

Here's a list of the most popular text editors that support Fortran syntax, in alphabetical order:
- [Atom](https://atom.io/)
- [Emacs](https://www.gnu.org/software/emacs/)
- [NotePad++](https://notepad-plus-plus.org/)
- [SublimeText](https://www.sublimetext.com/)
- [Vim](https://www.vim.org/) and [Neovim](https://neovim.io/)
- [Visual Studio Code](https://code.visualstudio.com/)

A comprehensive list with more choices is provided in [fortranwiki.org](http://fortranwiki.org/fortran/show/Source+code+editors).

Things to consider before choosing a text editor: 
- **Ergonomics:** This is purely subjective and concerns how easy, uninterrupted the UI (User Interface) feels to the developer while using the editor.
- **Extensibility:** As mentioned above, text editors come with many features as external packages. The variety, the installation process, the documentation, and user-friendliness of the packages all fall under this category.
- **Speed:** With how powerful modern hardware can be, the speed of editors is becoming of minor importance. However, for the less powerful systems, you may want to consider this as well. Heavyweight editors may impact the writing performance. For example, you can expect Atom and VSCode to run slower than the lightweight editors like Vim or Emacs.
- **Learning curve:** Last but not least, new users should consider how easy it is to get used to a new editor. Vim and Emacs have a steep learning curve and are notoriously difficult for newcomers. They offer a uniquely different experience than any other editor by forcing you to use the keyboard for all editing tasks.

## Configuring VS Code

With the editor opened, at the main UI, at the buttoned-column on the left, there is a *four-square-shaped* icon to open the Marketplace for extensions. 
Install [Modern Fortran](https://marketplace.visualstudio.com/items?itemName=krvajalm.linter-gfortran) for syntax highlighting, linting and Language Server support and formatting.

### Additional Tools

The following extensions are not essential but most users might find them useful:

- [GitLens](https://marketplace.visualstudio.com/items?itemName=eamodio.gitlens) for advanced Git visualization and operations.
- [CMake](https://marketplace.visualstudio.com/items?itemName=ms-vscode.cmake-tools) for full-featured CMake integration in VS Code.
- [Remote Development](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.vscode-remote-extensionpack) for working with containers, on a remote machines, or in the Windows Subsystem for Linux (WSL).
- [Even Better TOML by tamasfe](https://marketplace.visualstudio.com/items?itemName=tamasfe.even-better-toml). For Fortran developers that are using the new [fpm](https://github.com/fortran-lang/fpm) *Fortran Package Manager*, a TOML language support might come in handy.

## Configuring SublimeText

A well structured gist that provides instructions on how to set up SublimeText for Fortran developement is given [here](https://gist.github.com/sestelo/6b01e1405c1d0fa7f0ecdc951caaa995).

## Configuring Atom

Atom's configuration process is similar to VSCode one. At the main interface, if the *Welcome Guide* page is already opened for you, just click the **Install a Package** option, otherwise you can find the same option through the navigation bar at the top by clicking *Packages*. In doing so a new page under *Settings* opens where you can just start typing the package you want to install. 

One package that includes many Fortran features is [IDE-FORTRAN by hansec](https://atom.io/packages/ide-fortran). It needs the following packages to be installed:
- [atom-ide ui by facebook-atom](https://atom.io/packages/atom-ide-ui)
- [language-fortran by dparkins](https://atom.io/packages/language-fortran)

Additionally just like in VSCode it needs [Python](https://www.python.org/) and [Fortran Language Server](https://github.com/hansec/fortran-language-server) to be installed.

For version control a very popular package is [Git-Plus by akonwi](https://atom.io/packages/git-plus).

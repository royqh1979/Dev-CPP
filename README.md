# Dev-CPP
A fork of https://sourceforge.net/p/orwelldevcpp

Orwell Dev-CPP has stopped update since 2015, So I forked it. 
It's intended to be used for eductional purposes.

HighLights of News:
 * Greatly improved "Auto symbol completion" function. Autoskip matched )/}/]/"/', never need to delete it or skip it manually. This makes code input much fluent.
 * Fix auto-indent; When } is inputted, its line will intend to the same as the matching }.
 * Auto Code Completion fix:
   * In editor option dialog, user can choose to use Alt+/ instead of Ctrl+Space to call Code Completion Action. (In chinese systems, Ctrl-Space is used for switching input methods)
   * If there is only one code completion candidate, it is auto used and the suggestion form will not be displayed. This will speed the input.
   * Suggestion form can capture TAB keypress event now.
 * Debugger Improve:
   * Output window can hide all annotions of the output. This makes it much better to look, and we can use it just like using a gdb console.
   * If no breakpoint is set, the debugged program will pause at the entrance of main().
 * GDB 9.2 and GCC 9.2
 * User can open/edit/save/compile UTF-8 encoding files.
 * rename symbol ( using clang-rename)
 * -Wall -Wextra -Werror is setted by default in the Debug profile, to help beginners learn good coding habits.
 
And dozens of fixes and changes, see News.txt  

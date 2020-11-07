Red Panda Dev C++ （小熊猫Dev-C++，old name Dev-C++ 2000） is a new a improved fork of Orwell Dev-C++.

Orwell Dev-C++ has stopped updating since 2015, So I forked it. 

It's intended to be used for eductional purposes.

Website: https://royqh.net/devcpp-en/

中文网站在这里 https://royqh.net/devcpp/

HighLights of Red Panda Dev C++:
 * Greatly improved "Auto Code Completion":
   * Fixed header parsing error. （Can correctly show type hints for std::string, for example）
   * Auto code suggestion while typing.
   * Use Alt+/ instead of Ctrl+Space to call Code Completion Action.
   * Use TAB to finish completion.
 * Greatly improved Debugger:
   * breakpoints on condition
   * Redesigned Debugger panel, add Toolbar / Call Stack / Breakpoints sheet
   * Debug Toolbar
   * gdb Console
   * Infos in Watch View are updated timely
   * Rename vars in Watch View
 * Greatly improved ClassBrowser:
   * Correctly show #define/typedef/enum/class/struct/global var/function infos
   * sort by type/sort alphabetically
   * show/hide inherited members
   * correctly differentiate static class members / class members;
 * Greatly improved Code Parser, faster and less error;
 * Greatly improved "Auto symbol completion" function. (like in PyCharm/IDEA/CLion)
 * GDB 9.2 and GCC 9.2
 * View/editing/compile UTF-8 encoding files
 * use regular expressions in find/replace
 * Rename symbol in the editing file
 * -Wall -Wextra -Werror is setted by default in the Debug profile, to help beginners learn good coding habits.
 * redirect STDIN to a data file while running or debuging ( to easy debug / need a patched gdb ) 
 * Windows XP/ Windows 7/ Windows 10 Compatible
 * xege(graphics.h) and libturtle integration
 
And lots of fixes and changes, see News.txt  

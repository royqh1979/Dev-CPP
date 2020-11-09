// Execute & Pause
// Runs a program, then keeps the console window open after it finishes

#include <string>
using std::string;
#include <stdio.h>
#include <windows.h>

#define MAX_COMMAND_LENGTH 32768
#define MAX_ERROR_LENGTH 2048

LONGLONG GetClockTick() {
	LARGE_INTEGER dummy;
	QueryPerformanceCounter(&dummy);
	return dummy.QuadPart;
}

LONGLONG GetClockFrequency() {
	LARGE_INTEGER dummy;
	QueryPerformanceFrequency(&dummy);
	return dummy.QuadPart;
}

void PauseExit(int exitcode, bool reInp) {
    HANDLE hInp=NULL;
    if (reInp) {
        SECURITY_ATTRIBUTES sa;
        sa.nLength = sizeof(sa);
        sa.lpSecurityDescriptor = NULL;
        sa.bInheritHandle = TRUE;
		
        HANDLE hInp = CreateFile("CONIN$", GENERIC_WRITE | GENERIC_READ, 
            FILE_SHARE_READ , &sa, OPEN_EXISTING, /*FILE_ATTRIBUTE_NORMAL*/0, NULL);
            //si.hStdInput = hInp;
        SetStdHandle(STD_INPUT_HANDLE,hInp);
    }
	system("pause");
    if (reInp) {
        CloseHandle(hInp);
    }
	exit(exitcode);
}

string GetErrorMessage() {
	string result(MAX_ERROR_LENGTH,0);
	
	FormatMessage(
		FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL,GetLastError(),MAKELANGID(LANG_NEUTRAL,SUBLANG_DEFAULT),&result[0],result.size(),NULL);
	
	// Clear newlines at end of string
	for(int i = result.length()-1;i >= 0;i--) {
		if(isspace(result[i])) {
			result[i] = 0;
		} else {
			break;
		}
	}
	return result;
}

string GetCommand(int argc,char** argv,bool &reInp) {
	string result;
	reInp = (strcmp(argv[1],"0")!=0) ;
	for(int i = 2;i < argc;i++) {
/*
		// Quote the first argument in case the path name contains spaces
//		if(i == 1) {
//			result += string("\"") + string(argv[i]) + string("\"");
//		} else {
		// Quote the first argument in case the path name contains spaces
//		result += string(argv[i]);
//		}
*/ 
		// Quote the first argument in case the path name contains spaces
		result += string("\"") + string(argv[i]) + string("\"");
		
		// Add a space except for the last argument
		if(i != (argc-1)) {
			result += string(" ");
		}
	}
	
	if(result.length() > MAX_COMMAND_LENGTH) {
		printf("\n--------------------------------");
		printf("\nError: Length of command line string is over %d characters\n",MAX_COMMAND_LENGTH);
		PauseExit(EXIT_FAILURE,reInp);
	}
	
	return result;
}

DWORD ExecuteCommand(string& command,bool reInp) {
	STARTUPINFO si;
	PROCESS_INFORMATION pi;
	memset(&si,0,sizeof(si));
	si.cb = sizeof(si);
	memset(&pi,0,sizeof(pi));

	
	if(!CreateProcess(NULL, (LPSTR)command.c_str(), NULL, NULL, true, 0, NULL, NULL, &si, &pi)) {
		printf("\n--------------------------------");
		printf("\nFailed to execute \"%s\":",command.c_str());
		printf("\nError %lu: %s\n",GetLastError(),GetErrorMessage().c_str());
		PauseExit(EXIT_FAILURE,reInp);
	}
	WaitForSingleObject(pi.hProcess, INFINITE); // Wait for it to finish
	
	DWORD result = 0;
	GetExitCodeProcess(pi.hProcess, &result);
	return result;
}

int main(int argc, char** argv) {
	
	// First make sure we aren't going to read nonexistent arrays
	if(argc < 3) {
		printf("\n--------------------------------");
		printf("\nUsage: ConsolePauser.exe <0|1> <filename> <parameters>\n");
		printf("\n 1 means the STDIN is redirected by Dev-CPP;0 means not\n");
		PauseExit(EXIT_SUCCESS,false);
	}
	
	// Make us look like the paused program
	SetConsoleTitle(argv[2]);
	
	bool reInp;
	// Then build the to-run application command
	string command = GetCommand(argc,argv,reInp);

	// Save starting timestamp
	LONGLONG starttime = GetClockTick();
	
	// Then execute said command
	DWORD returnvalue = ExecuteCommand(command,reInp);
	
	// Get ending timestamp
	LONGLONG endtime = GetClockTick();
	double seconds = (endtime - starttime) / (double)GetClockFrequency();

	// Done? Print return value of executed program
	printf("\n--------------------------------");
	printf("\nProcess exited after %.4g seconds with return value %lu\n",seconds,returnvalue);
	PauseExit(returnvalue,reInp);
}

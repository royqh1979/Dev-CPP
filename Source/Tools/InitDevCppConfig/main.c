#ifndef _WIN32_IE
#define _WIN32_IE 0x0400
#endif
#include <windows.h>
#include <shlobj.h>
#include <stdio.h>
#include "resource.h"

HINSTANCE hInst;

BOOL MainDlgProc(HWND hDlg, UINT Msg, WPARAM wParam, LPARAM lParam);

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpCmdLine, int nCmdShow) {
	MSG msg;
	HWND hMainDlg = NULL;

	hInst = hInstance;
	hMainDlg = CreateDialog(hInstance, (LPCTSTR)IDD_MAIN_DIALOG, 0,(DLGPROC)MainDlgProc);
	ShowWindow(hMainDlg, nCmdShow);
	while (GetMessage(&msg, NULL, 0, 0)) {
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}
	return 0;
}

BOOL writeConfigFile(const char* filepath, int res_id) {
	HRSRC hRes;
	HGLOBAL hResLoad;
	LPVOID lpRes;
	DWORD nRes;
	FILE* fp;
	hRes = FindResource(hInst, MAKEINTRESOURCE(res_id), "INI");
	if (hRes==NULL ) {
		return FALSE;
	}
	hResLoad=LoadResource(hInst,hRes) ;
	if (hResLoad==NULL) {
		return FALSE;
	}
	lpRes=LockResource(hResLoad);
	if (lpRes==NULL) {
		return FALSE;
	}
	nRes=SizeofResource(hInst,hRes);
	
	fp=fopen(filepath,"w");
	if (fp==NULL) {
		return FALSE;
	}
	fwrite(lpRes,nRes,1,fp);
	fclose(fp);
	return TRUE;
}

BOOL initDevCppConfig(HWND hDlg) {
	char configDir[MAX_PATH+1];
	char dirPath[MAX_PATH+1];
	char filePath[MAX_PATH+1];
	HANDLE hFind;
	BOOL result;
	WIN32_FIND_DATA fileFindData;
	
	
	//获取DEV-CPP所在目录 
	SHGetSpecialFolderPath(NULL,configDir,CSIDL_APPDATA,FALSE);
	strcat(configDir,"\\Dev-CPP");
	
	sprintf(dirPath,"%s\\*.*",configDir);	
	hFind=FindFirstFile(dirPath,&fileFindData);
	result=(hFind!=INVALID_HANDLE_VALUE);
	while (result) {
		if (!(fileFindData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) {
			sprintf(filePath,"%s\\%s",configDir,fileFindData.cFileName);			
			if (!DeleteFile(filePath)) {
				char errorMessage[1000+1]; 
				char errorMessage2[1100+1];
				DWORD error=GetLastError();
				FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM,NULL,error,0,errorMessage,1000,NULL);
				sprintf(errorMessage2,"目录 %s 删除失败，原因是：\n %s",dirPath,errorMessage); 
				MessageBox(hDlg,errorMessage2,"初始化失败！",MB_OK); 	
				return FALSE;
			}
		}
		result=FindNextFile(hFind,&fileFindData);
	}
	/*
	if(!RemoveDirectory(configDir)){
		return FALSE;
	} 
	*/
	
	sprintf(filePath,"%s\\%s",configDir,"devcpp.ini");
	if (!writeConfigFile(filePath, IDR_DEVCPP)) {
		char errorMessage[1000+1]; 
		char errorMessage2[1100+1];
		DWORD error=GetLastError();
		FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM,NULL,error,0,errorMessage,1000,NULL);
		sprintf(errorMessage2,"创建配置文件 %s 失败，原因是：\n %s",filePath,errorMessage); 
		MessageBox(hDlg,errorMessage2,"初始化失败！",MB_OK); 
		return FALSE;
	}
	sprintf(filePath,"%s\\%s",configDir,"codeinsertion.ini");
	if (!writeConfigFile(filePath, IDR_CODE_INSERTION)) {
		char errorMessage[1000+1]; 
		char errorMessage2[1100+1];
		DWORD error=GetLastError();
		FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM,NULL,error,0,errorMessage,1000,NULL);
		sprintf(errorMessage2,"创建配置文件 %s 失败，原因是：\n %s",filePath,errorMessage); 
		MessageBox(hDlg,errorMessage2,"初始化失败！",MB_OK);
		return FALSE;
	}

	return TRUE;
}

BOOL MainDlgProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam) {
	switch (message) {
		case WM_INITDIALOG :
			return TRUE ;
		case WM_COMMAND :
			switch (LOWORD (wParam)) {
				case IDOK:
					if (initDevCppConfig(hDlg)) {
						MessageBox(hDlg,"初始化成功！请重新启动Dev-Cpp","初始化成功",MB_OK); 
					}
					DestroyWindow(hDlg);
					return TRUE;
				case IDCANCEL :
					DestroyWindow(hDlg);
					return TRUE ;
			}
			break ;
		case WM_CLOSE:
			DestroyWindow(hDlg);
			return TRUE;
		case WM_DESTROY:
			PostQuitMessage(0);
			return TRUE;
	};
	return FALSE;//返回FALSE给缺省对话框函数DefDlgProc(),表示没有处理本消息
}

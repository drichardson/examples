#define UNICODE
#define _UNICODE
#define WIN32_LEAN_AND_MEAN
#define WINVER _WIN32_WINNT_WIN10

#include <windows.h>
#include <stdio.h>

void wmain(int argc, wchar_t* argv[])
{
    WIN32_FIND_DATA FindFileData;
    HANDLE hFind;

    if (argc != 2)
    {
        wprintf(L"Usage: %ls [target_file]\n", argv[0]);
        return;
    }

    wprintf(L"Target file is %ls\n", argv[1]);
    hFind = FindFirstFileExW(argv[1], FindExInfoStandard, &FindFileData, FindExSearchNameMatch, NULL, 0);
    if (hFind == INVALID_HANDLE_VALUE) 
    {
        wprintf(L"FindFirstFileEx failed (%d)\n", (int)GetLastError());
        return;
    } 
    else 
    {
        wprintf(L"The first file found is %ls\n", FindFileData.cFileName);
        FindClose(hFind);
    }
}


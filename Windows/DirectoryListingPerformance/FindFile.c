#define UNICODE
#define _UNICODE
#define WIN32_LEAN_AND_MEAN
#define WINVER _WIN32_WINNT_WIN10

#include <stdio.h>
#include <strsafe.h>
#include <windows.h>

// https://docs.microsoft.com/en-us/windows/win32/fileio/naming-a-file#maximum-path-length-limitation
#define LONG_MAX_PATH 32800

int ls(const wchar_t* path);

int wmain(int argc, wchar_t* argv[])
{
    WIN32_FIND_DATA FindFileData;
    HANDLE hFind;

    if (argc != 2)
    {
        wprintf(L"Usage: %ls <DIR>\n", argv[0]);
        return 1;
    }

    int total = ls(argv[1]);
    wprintf(L"TOTAL: %d\n", total);

    return 0;
}

int ls(const wchar_t* filename)
{
    wchar_t scratch[MAX_PATH];
    HRESULT ok = StringCbPrintfW(scratch, sizeof(scratch), L"%ls\\*.*", filename);
    if (ok != S_OK) {
        wprintf(L"ERROR: StringCbPrintfW failed in ls.\n");
        return -1;
    }

    wprintf(L"Looking in %ls\n", scratch);
    WIN32_FIND_DATA FindFileData;
    HANDLE hFind = FindFirstFileExW(scratch,
            FindExInfoBasic, &FindFileData,
            FindExSearchNameMatch,
            NULL,
            FIND_FIRST_EX_CASE_SENSITIVE | FIND_FIRST_EX_LARGE_FETCH);

    if (hFind == INVALID_HANDLE_VALUE) 
    {
        wprintf(L"FindFirstFileEx failed (%d)\n", (int)GetLastError());
        return -1;
    } 

    int total = 1;
    do
    {
        if (FindFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
        {
            if (wcscmp(FindFileData.cFileName, L".") == 0 || wcscmp(FindFileData.cFileName, L"..") == 0) {
                continue;
            }

            wprintf(L"D: %ls\n", FindFileData.cFileName);
            HRESULT ok = StringCbPrintfW(scratch, sizeof(scratch), L"%ls\\%ls", filename, FindFileData.cFileName);

            int subtotal = ls(scratch);
            if (subtotal == -1) {
                wprintf(L"ERROR IN SUBDIR: %s\n", FindFileData.cFileName);
                return -1;
            }

            total += subtotal;
        }
        else
        {
            wprintf(L"F: %ls\n", FindFileData.cFileName);
            total++;
        }
    }
    while(FindNextFileW(hFind, &FindFileData));

    FindClose(hFind);
    return total;
}


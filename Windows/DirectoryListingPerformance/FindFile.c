#include <Windows.h>
#include <stdio.h>
#include <strsafe.h>

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

    wprintf(L"%ls\n", scratch);
    WIN32_FIND_DATA FindFileData;
    HANDLE hFind = FindFirstFileExW(scratch,
            FindExInfoBasic, &FindFileData,
            FindExSearchNameMatch,
            NULL,
            FIND_FIRST_EX_CASE_SENSITIVE | FIND_FIRST_EX_LARGE_FETCH);

    if (hFind == INVALID_HANDLE_VALUE) 
    {
        wprintf(L"Warning: (%d): Path=%ls\n", (int)GetLastError(), scratch);
        return 0;
    } 

    int total = 1;
    do
    {
        if (FindFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
        {
            if (wcscmp(FindFileData.cFileName, L".") == 0 || wcscmp(FindFileData.cFileName, L"..") == 0) {
                continue;
            }

            total++;

            wprintf(L"D: %ls\n", FindFileData.cFileName);

            HRESULT ok = StringCbPrintfW(scratch, sizeof(scratch), L"%ls\\%ls", filename, FindFileData.cFileName);
            if (ok != S_OK) {
                wprintf(L"ERROR: StringCbPrintfW failed on %ls", filename);
                continue;
            }

            total += ls(scratch);
        }
        else
        {
            total++;
            wprintf(L"F: %ls\n", FindFileData.cFileName);
        }
    }
    while(FindNextFileW(hFind, &FindFileData));

    FindClose(hFind);
    return total;
}


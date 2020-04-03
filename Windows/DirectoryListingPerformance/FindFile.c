#include <windows.h>
#include <tchar.h>
#include <stdio.h>

void _tmain(int argc, TCHAR *argv[])
{
    WIN32_FIND_DATA FindFileData;
    HANDLE hFind;

    if( argc != 2 )
    {
        _tprintf(TEXT("Usage: %s [target_file]\n"), argv[0]);
        return;
    }

    _tprintf (TEXT("Target file is %s\n"), argv[1]);
    hFind = FindFirstFileEx(argv[1], FindExInfoStandard, &FindFileData,
            FindExSearchNameMatch, NULL, 0);
    if (hFind == INVALID_HANDLE_VALUE) 
    {
        printf ("FindFirstFileEx failed (%d)\n", GetLastError());
        return;
    } 
    else 
    {
        _tprintf (TEXT("The first file found is %s\n"), 
                FindFileData.cFileName);
        FindClose(hFind);
    }
}


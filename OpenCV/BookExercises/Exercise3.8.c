//
//  Exercise3.8.c

#include <stdio.h>
#include <opencv2/highgui/highgui_c.h>
#include <opencv2/imgproc/imgproc_c.h>
#include <opencv2/core/core_c.h>

typedef struct my_struct {
	int i;
	CvPoint point;
	CvRect rect;
} my_struct;

static void write_my_struct( CvFileStorage* fs, const char* name, const my_struct *ms)
{
	cvStartWriteStruct(fs, name, CV_NODE_MAP, NULL, cvAttrList(NULL, NULL));
		cvWriteInt(fs, "i", ms->i);
		cvStartWriteStruct(fs, "point", CV_NODE_MAP, NULL, cvAttrList(NULL, NULL));
		cvWriteInt(fs, "x", ms->point.x);
		cvWriteInt(fs, "y", ms->point.y);
		cvEndWriteStruct(fs);
		cvStartWriteStruct(fs, "rect", CV_NODE_MAP, NULL, cvAttrList(NULL, NULL));
			cvWriteInt(fs, "x", ms->rect.x);
			cvWriteInt(fs, "y", ms->rect.y);
			cvWriteInt(fs, "width", ms->rect.width);
			cvWriteInt(fs, "height", ms->rect.height);
		cvEndWriteStruct(fs);
	cvEndWriteStruct(fs);
}

static void read_my_struct( CvFileStorage* fs, CvFileNode* ms_node, my_struct *ms)
{
	ms->i = cvReadIntByName(fs, ms_node, "i", 0);
	
	CvFileNode* pointNode = cvGetFileNodeByName(fs, ms_node, "point");
	if ( pointNode )
	{
		ms->point = cvPoint(cvReadIntByName(fs, pointNode, "x", 0), cvReadIntByName(fs, pointNode, "y", 0));
	}
	
	CvFileNode* rectNode = cvGetFileNodeByName(fs, ms_node, "rect");
	if ( rectNode )
	{
		ms->rect = cvRect(cvReadIntByName(fs, rectNode, "x", 0), cvReadIntByName(fs, rectNode, "y", 0), cvReadIntByName(fs, rectNode, "width", 0), cvReadIntByName(fs, rectNode, "height", 0));
	}
}

int main (int argc, const char * argv[])
{
	my_struct ms;
	bzero(&ms, sizeof(ms));
	
	CvFileStorage* fs = cvOpenFileStorage("my_struct.yml", 0, CV_STORAGE_READ);
	
	if ( fs )
	{
		puts("opened file");
		CvFileNode* ms_node = cvGetFileNodeByName(fs, NULL, "my_struct");
		
		if ( ms_node )
		{
			read_my_struct(fs, ms_node, &ms);
	
			ms.i += 1;
			ms.point = cvPoint(ms.point.x + 1, ms.point.y + 1);
			ms.rect = cvRect(ms.rect.x + 1, ms.rect.y + 1, ms.rect.width + 1, ms.rect.height + 1);
		}
		
		cvReleaseFileStorage( &fs );
		fs = NULL;
	}
	
	fs = cvOpenFileStorage("my_struct.yml", 0, CV_STORAGE_WRITE);
	
	if ( fs == NULL )
	{
		fprintf(stderr, "Error writing out file.\n");
		exit(1);
	}
	
	write_my_struct(fs, "my_struct", &ms);
	
	cvReleaseFileStorage( &fs );
	
	puts("Wrote out my_struct");
	
	system("cat my_struct.yml");

    return 0;
}


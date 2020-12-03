#define _POSIX_C_SOURCE 199309L

#include <curl/curl.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

uint64_t last_time;

uint64_t now(void)
{
	struct timespec ts;
	if (clock_gettime(CLOCK_MONOTONIC, &ts))
	{
		fprintf(stderr, "clock_gettime failed\n");
		exit(1);
	}
	return ts.tv_sec * 1000000000 + ts.tv_nsec;
}

void print_interval(const char *msg)
{
	uint64_t cur = now();
	double interval_ns = cur - last_time;
	double interval = interval_ns / 1e9;

	printf("TIME ELAPSED: %fs. %s\n", interval, msg);

	last_time = cur;
}

void check(bool ok, char const *function)
{
	if (!ok)
	{
		fprintf(stderr, "%s failed\n", function);
		exit(1);
	}

	print_interval(function);
}

struct report_t
{
	size_t total_size;
	size_t total_nmemb;
	int callback_count;
};

size_t write_data(void const *buffer, size_t size, size_t nmemb, void *userp)
{
	struct report_t *report = userp;
	report->total_size += size;
	report->total_nmemb += nmemb;

	// fwrite(buffer, size, nmemb, stdout);
	return size * nmemb;
}

int main(int argc, char **argv)
{
	last_time = now();

	CURLcode rc = curl_global_init(CURL_GLOBAL_DEFAULT);
	check(rc == CURLE_OK, "curl_global_init");

	CURL *curl = curl_easy_init();
	check(curl, "curl_easy_init");

	rc = curl_easy_setopt(curl, CURLOPT_URL, "https://dougrichardson.us");
	check(rc == CURLE_OK, "curl_easy_setopt(url)");

	rc = curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
	check(rc == CURLE_OK, "curl_easy_setop(writefunction)");

	struct report_t report = {0};
	rc = curl_easy_setopt(curl, CURLOPT_FILE, &report);
	check(rc == CURLE_OK, "curl_easy_setopt(writedata)");

	rc = curl_easy_perform(curl);
	check(rc == CURLE_OK, "curl_easy_perform");

	curl_easy_cleanup(curl);
	check(true, "curl_easy_cleanup");

	printf("REPORT:\n"
	       " total size: %zu\n"
	       " total nmemb: %zu\n"
	       " total bytes: %zu\n",
	       report.total_size,
	       report.total_nmemb,
	       report.total_size * report.total_nmemb);

	return 0;
}

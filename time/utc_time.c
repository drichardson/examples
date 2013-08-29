#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

static time_t mktime_in_zone(struct tm *t, char const* timezone) {
    char const *orig = getenv("TZ");
    setenv("TZ", timezone, 1);
    time_t result = mktime(t);
    if (orig) {
        setenv("TZ", orig, 1);
    } else {
        unsetenv("TZ");
    }
    return result;
}

static time_t fake_timegm(struct tm *t) {
    return mktime_in_zone(t, "UTC");
}

static time_t fake_timegm_without_modifying_env(struct tm *t) {
    time_t zero_time = 0;
    struct tm zero_tm;
    gmtime_r(&zero_time, &zero_tm);

    time_t offset_from_utc = mktime(&zero_tm);
    time_t result = mktime(t);

    return result - offset_from_utc;
}

static void test_time_string(char const* time_string, char const* time_format, time_t expected) {
    printf("*** Testing time %s, format %s, expected unix time %ld **********\n", time_string, time_format, expected);
    
    struct tm tm;
    memset(&tm, 0, sizeof(tm));
    char const* rc = strptime(time_string, time_format, &tm);
    if (rc == NULL || *rc != '\0') {
        fprintf(stderr, "Failed to parse time string. Result %s\n", rc);
        exit(1);
    }

    struct tm tm_for_mktime = tm;
    time_t mktime_time = mktime(&tm_for_mktime);

    struct tm tm_for_mktime_in_zone = tm;
    time_t mktime_in_zone_time = mktime_in_zone(&tm_for_mktime_in_zone, "EST");

    struct tm tm_for_fake = tm;
    time_t fake_time = fake_timegm(&tm_for_fake);

    struct tm tm_for_fake_without_modifying_env = tm;
    time_t fake_time_without_modifying_env = fake_timegm_without_modifying_env(&tm_for_fake_without_modifying_env);

    struct tm tm_for_gm = tm;
    time_t gm_time = timegm(&tm_for_gm);

    printf("fake_timegm changed tm? %d\n", memcmp(&tm, &tm_for_fake, sizeof(tm)));
    printf("timegm changed tm? %d\n", memcmp(&tm, &tm_for_gm, sizeof(tm)));
    printf("fake_timegm tm compare to timegm tm: %d\n", memcmp(&tm_for_fake, &tm_for_gm, sizeof(tm_for_fake)));

    printf("mktime                = %ld\n", mktime_time);
    printf("mktime_in_zone EST    = %ld\n", mktime_in_zone_time);
    printf("fake_time             = %ld\n", fake_time);
    printf("fake_time no modify   = %ld\n", fake_time_without_modifying_env);
    printf("gm_time               = %ld\n", gm_time);
    printf("gm_time using offset  = %ld\n", gm_time - tm.tm_gmtoff);
}
int main(int argc, char** argv) {
    test_time_string("1970-01-01T00:00:00Z", "%FT%TZ", 0);
    test_time_string("2013-08-28T17:58:53Z", "%FT%TZ", 1377712733);
    test_time_string("2013-08-29T09:16:23-0700", "%FT%T%z", 1377792983);
    return 0;
}



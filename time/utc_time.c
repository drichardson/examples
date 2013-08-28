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
    struct tm zero_tm;
    memset(&zero_tm, 0, sizeof(zero_tm));
    zero_tm.tm_sec = 0;
    zero_tm.tm_min = 0;
    zero_tm.tm_hour = 0;
    zero_tm.tm_mday = 1; // 1 based
    zero_tm.tm_mon = 0; // 0 based
    zero_tm.tm_year = 1970 - 1900; // 1900 based
    zero_tm.tm_yday = 0; // 0 based
    zero_tm.tm_isdst = 0;
    zero_tm.tm_gmtoff = 0;

    time_t offset_from_utc = mktime(&zero_tm);
    time_t result = mktime(t);

    return result - offset_from_utc;
}

static void test_time_string(char const* time_string, time_t expected) {
    printf("*** Testing time %s, expected unix time %ld **********\n", time_string, expected);
    
    struct tm tm;
    memset(&tm, 0, sizeof(tm));
    char const* rc = strptime(time_string, "%FT%TZ", &tm);
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
}
int main(int argc, char** argv) {
    test_time_string("1970-01-01T00:00:00Z", 0);
    test_time_string("2013-08-28T17:58:53Z", 1377712733);
    return 0;
}



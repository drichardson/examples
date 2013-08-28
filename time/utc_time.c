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

int main(int argc, char** argv) {
    
    struct tm tm;
    memset(&tm, 0, sizeof(tm));
    char const* rc = strptime("1970-01-01T00:00:00Z", "%FT%TZ", &tm);
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

    struct tm tm_for_gm = tm;
    time_t gm_time = timegm(&tm_for_gm);

    printf("fake_timegm changed tm? %d\n", memcmp(&tm, &tm_for_fake, sizeof(tm)));
    printf("timegm changed tm? %d\n", memcmp(&tm, &tm_for_gm, sizeof(tm)));
    printf("fake_timegm tm compare to timegm tm: %d\n", memcmp(&tm_for_fake, &tm_for_gm, sizeof(tm_for_fake)));

    printf("mktime                = %ld\n", mktime_time);
    printf("mktime_in_zone EST    = %ld\n", mktime_in_zone_time);
    printf("fake_time             = %ld\n", fake_time);
    printf("gm_time               = %ld\n", gm_time);

    return 0;
}

#include <glog/logging.h>

int main(int argc, char* argv[]) {
    google::InitGoogleLogging(argv[0]);
    FLAGS_minloglevel = google::GLOG_INFO;
    FLAGS_logtostderr = true;
    LOG(INFO) << "Found " << 123;
    for(int i = 0; i < 100; ++i) {
        LOG_EVERY_N(INFO, 10) << "Logging every 10 " << i;
    }
    LOG_IF(INFO, true) << "LOG_IF with true";
    LOG_IF(INFO, false) << "LOG_IF with false";
    for(int i = 0; i < 20; ++i) {
        LOG_FIRST_N(INFO, 2) << "Logging first 2 " << i;
    }
}


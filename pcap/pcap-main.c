// pcap was complaining until I defined _BSD_SOURCE. _GNU_SOURCE also seems to work.
#define _BSD_SOURCE

#include <pcap/pcap.h>
#include <stdio.h>
#include <stdlib.h>

static void
die(char const* msg) {
    fputs(msg, stderr);
    exit(1);
}

static void
print_list_of_capture_devices(void) {
    char errbuf[PCAP_ERRBUF_SIZE];
    pcap_if_t* if_list = NULL;
    int rc = pcap_findalldevs(&if_list, errbuf);
    if (rc != 0) {
        fprintf(stderr, "pcap_findalldevs failed %d. %s\n", rc, errbuf);
        exit(1);
    }
    printf("Capture devices =====\n");
    for(pcap_if_t *i = if_list; i != NULL; i = i->next) {
        printf("  %s:%s\n", i->name, i->description);
    }
    pcap_freealldevs(if_list);
}

static void
print_list_of_timestamps_for_pcap(pcap_t* pc) {
    int* ts_types = NULL;
    int ts_count = 0;
    ts_count = pcap_list_tstamp_types(pc, &ts_types);
    if (ts_count == PCAP_ERROR) die("pcap_list_tstamp_types failed");
    if (ts_count == 0) puts("WARNING: you aren't allowed to set the tstamp type for this pcap device");
    puts("Timestamp types=====");
    for(int i = 0; i < ts_count; ++i) {
        printf("  %d: %s\n", ts_types[i], pcap_tstamp_type_val_to_name(ts_types[i]));
    }
    pcap_free_tstamp_types(ts_types);
}

static void
print_hex_dump(unsigned char const* data, size_t len) {
    char const nibble[] ={
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
        'A', 'B', 'C', 'D', 'E', 'F' };

    for(size_t i = 0; i < len; ++i) {
       unsigned char b = data[i]; 
       putchar(nibble[b >> 4]);
       putchar(nibble[b & 15]);
    }
}

static int
min(int a, int b) {
    return a < b ? a : b;
}

int main(int argc, char const** argv) {
    char errbuf[PCAP_ERRBUF_SIZE];
    int rc;
    char const* source = "eth0";

    printf("libpcap version: %s\n", pcap_lib_version());
    print_list_of_capture_devices();

    //
    // Create new pcap
    //
    printf("Using source %s\n", source);
    pcap_t* pc = pcap_create(source, errbuf);
    if (pc == NULL) {
        fprintf(stderr, "pcap_create failed on source %s. %s\n", source, errbuf);
        exit(1);
    }   

    print_list_of_timestamps_for_pcap(pc);

    //
    // Configure pcap options
    //
    rc = pcap_set_snaplen(pc, 100);
    if (rc != 0) die("pcap_set_snaplen failed");
    rc = pcap_set_promisc(pc, 0);
    if (rc != 0) die("pcap_set_promisc failed");
    // 1Gbit/1s * 1s/1000ms = 1Mbit/1ms. Therefore, on a 1Gbps network, each ms
    // of read delay can fill up 1Mbit of buffer which is
    // 1Mbit*(1byte/8bit)=125k bytes per ms.
    int const read_timeout_ms = 1000;
    rc = pcap_set_timeout(pc, read_timeout_ms);
    if (rc != 0) die("pcap_set_timeout failed");
    rc = pcap_set_buffer_size(pc, 125000 * read_timeout_ms);
    if (rc != 0) die("pcap_set_buffer_size failed");

#if 0
    // DOUG: Seeing weird bug here. print_list_of_timestamps_for_pcap indicates
    // I can set the tstamp type to PCAP_TSTAMP_ADAPTER but when I do so, pcap_activate
    // fails with 3 (PCAP_WARNING_TSTAMP_TYPE_NOTSUP). I'm running tests on VMWare Fusion 7 Pro using
    // Linux arch1 3.15.5-1-ARCH #1 SMP PREEMPT Thu Jul 10 07:08:50 CEST 2014 x86_64 GNU/Linux
    // and libpcap version 1.5.3.

    rc = pcap_set_tstamp_type(pc, PCAP_TSTAMP_ADAPTER); 
    if (rc != 0) die("pcap_set_tstamp_type failed");
#endif

    //
    // Start capturing
    //
    rc = pcap_activate(pc);
    if (rc != 0) {
        fprintf(stderr, "pcap_activate returned %d. %s\n", rc, pcap_geterr(pc));
        exit(1);
    }

    puts("Capture source activated");
    struct pcap_pkthdr* hdr = NULL;
    const u_char* data = NULL;
    while(1) {
        rc = pcap_next_ex(pc, &hdr, &data);
        switch(rc) {
        case 1: // packet read
            printf("read packet: caplen=%4d, len=%4d data=", hdr->caplen, hdr->len);
            print_hex_dump(data, min(hdr->caplen, 50));
            putchar('\n');
            break;
        case 0: // timeout expired
            break;
        case -1: // error occurred while reading packet
            fprintf(stderr, "Error occurred while reading packet. %s", pcap_geterr(pc));
            exit(1);
            break;
        case -2: // no more packets in savefile. 
            // Since we're live capturing from device, shouldn't hit this.
            puts("No more packets in savefile.");
            break;
        default:
            fprintf(stderr, "Unexpected pcap_next_ex return value %d\n", rc);
            exit(1);
            break;
        }
    } 

    pcap_close(pc);
    return 0;
}

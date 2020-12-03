// pcap was complaining until I defined _BSD_SOURCE. _GNU_SOURCE also seems to work.
#define _BSD_SOURCE

#include <pcap/pcap.h>
#include <stdbool.h>
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

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;

static u16
u8s_to_u16(u8 const u[2]) {
    u16 r0 = u[0], r1 = u[1];
    return (r0 << 8) | r1;
}

#if 0
static u32
u8s_to_u32(u8 const u[4]) {
    u32 r0 = u[0], r1 = u[1], r2 = u[2], r3 = u[4];
    return (r0 << 24) | (r1 << 16) | (r2 << 8) | r3;
}
#endif

struct ethernet_frame {
    u8 dst_mac_address[6];
    u8 src_mac_address[6];
    u8 ethertype[2];
};

u16 const ETHERTYPE_IPV4 = 0x0800;
u16 const ETHERTYPE_ARP = 0x0806;
u16 const ETHERTYPE_WAKE_ON_LAN = 0x0842;
u16 const ETHERTYPE_IETF_TRILL = 0x22F3;
u16 const ETHERTYPE_DECNET_PHASE_IV = 0x6003;
u16 const ETHERTYPE_REVERSE_ARP = 0x8035;
u16 const ETHERTYPE_APPLETALK = 0x809B;
u16 const ETHERTYPE_APPLETALK_ARP = 0x80F3;
u16 const ETHERTYPE_VLAN_TAGGED_FRAME = 0x8100;
u16 const ETHERTYPE_IPX_1 = 0x8137;
u16 const ETHERTYPE_IPX_2 = 0x8138;
u16 const ETHERTYPE_QNX_QNET = 0x8204;
u16 const ETHERTYPE_IPV6 = 0x86DD;
u16 const ETHERTYPE_ETHERNET_FLOW_CONTROL = 0x8808;
u16 const ETHERTYPE_SLOW_PROTOCOLS = 0X8809;
u16 const ETHERTYPE_COBRANET = 0x8819;
u16 const ETHERTYPE_MPLS_UNICAT = 0x8847;
u16 const ETHERTYPE_MPLS_MULTICAST = 0x8848;
u16 const ETHERTYPE_PPPOE_DISCOVERY_STAGE = 0x8863;
u16 const ETHERTYPE_PPPOE_SESSION_STAGE = 0x8864;
u16 const ETHERTYPE_JUMBO_FRAMES = 0x8870;
u16 const ETHERTYPE_HOMEPLUG_1DOT0_MME = 0x887B;
u16 const ETHERTYPE_EAP_OVER_LAN = 0x888E;
u16 const ETHERTYPE_PROFINET_PROTOCOL = 0x8892;
u16 const ETHERTYPE_HYPERSCSI = 0x889A;
u16 const ETHERTYPE_ATA_OVER_ETHERNET = 0x88A2;
u16 const ETHERTYPE_ETHERCAT_PROTOCOL = 0x88A4;
u16 const ETHERTYPE_PROVIDER_BRIDGING = 0x88A8;
u16 const ETHERTYPE_ETHERNET_POWERLINK = 0x88AB;
u16 const ETHERTYPE_LINK_LAYER_DISCOVERY_PROTOCOL = 0x88CC;
u16 const ETHERTYPE_SERCOS_III = 0x88CD;
u16 const ETHERTYPE_HOMEPLUG_AV_MME = 0x88E1;
u16 const ETHERTYPE_MEDIA_REDUNDANCY_PROTOCOL = 0x88E3;
u16 const ETHERTYPE_MAC_SECURITY = 0x88E5;
u16 const ETHERTYPE_PRECISION_TIME_PROTOCOL = 0x88F7;
u16 const ETHERTYPE_IEEE_CONNECTIVITY_FAULT_MANAGEMENT_PROTOCOL = 0x8902;
u16 const ETHERTYPE_FIBRE_CHANNEL_OVER_ETHERNET = 0x8906;
u16 const ETHERTYPE_FCOE_INITIALIZATION_PROTOCOL = 0x8914;
u16 const ETHERTYPE_RDMA_OVER_CONVERGED_ETHERNET = 0x8915;
u16 const ETHERTYPE_HIGH_AVAILABILITY_SEAMLESS_REDUNDANCY = 0x892F;
u16 const ETHERTYPE_ETHERNET_CONFIGURATION_TESTING_PROTOCOL= 0x9000;
u16 const ETHERTYPE_Q_I_Q = 0x9100;
u16 const ETHERTYPE_VERITAS_LOW_LATENCY_TRANSPORT = 0xCAFE;

static void
mac_address_print(u8 const a[6]) {
    printf("%02X:%02X:%02X:%02X:%02X:%02X", a[0], a[1], a[2], a[3], a[4], a[5]);
}

static void
ethernet_frame_print(struct ethernet_frame const* e) {
    printf("Ethernet: src_mac=");
    mac_address_print(e->src_mac_address);
    printf(", dst_mac=");
    mac_address_print(e->dst_mac_address);
    printf(", ethertype=0x%hu", u8s_to_u16(e->ethertype));
}

struct ipv4_packet {
    u8 version_ihl;
    u8 dscp_ecn;
    u8 total_length[2];
    u8 identification[2];
    u8 flags_fragment_offset[2];
    u8 time_to_live;
    u8 protocol;
    u8 header_checksum[2];
    u8 src_ip_address[4];
    u8 dst_ip_address[4];
    u8 options[0]; // if ihl > 5
};

#define IPV4_PROTOCOL_ICMP 1
#define IPV4_PROTOCOL_IGMP 2
#define IPV4_PROTOCOL_TCP 6
#define IPV4_PROTOCOL_UDP 17
#define IPV4_PROTOCOL_ENCAP 41
#define IPV4_PROTOCOL_OSPF 89
#define IPV4_PROTOCOL_SCTP 132

static void
ipv4_address_print(u8 const addr[4]) {
    printf("%hhu.%hhu.%hhu.%hhu", addr[0], addr[1], addr[2], addr[4]);
}

static u8
ipv4_packet_ihl(struct ipv4_packet const* p) {
    return p->version_ihl & 0xf;
}

static void
ipv4_packet_print(struct ipv4_packet const* p) {
    printf("IPv4: ihl=%hhu", ipv4_packet_ihl(p));
    printf(", total_len=%hu", u8s_to_u16(p->total_length));
    printf(", protocol=%hhX", p->protocol);
    printf(", src_ip=");
    ipv4_address_print(p->src_ip_address);
    printf(", dst_ip=");
    ipv4_address_print(p->src_ip_address);
}

struct udp_packet {
    u8 src_port[2];
    u8 dst_port[2];
    u8 length[2];
    u8 checksum[2];
};

static void udp_packet_print(struct udp_packet const* udp) {
    printf("UDP: src_port=%hu, dst_port=%hu", u8s_to_u16(udp->src_port),
            u8s_to_u16(udp->dst_port));
}

struct tcp_packet {
    u8 src_port[2];
    u8 dst_port[2];
    u8 sequence_number[4];
    u8 acknowledgement_number[4];
    u8 data_offset_reserved_flag1;
    u8 flag2_thru_9;
    u8 window_size[2];
    u8 checksum[2];
    u8 urgent_pointer[2];
    u8 options[0]; // if data offset > 5
};

static void
tcp_packet_print(struct tcp_packet const* tcp) {
    printf("TCP: src_port=%hu, dst_port=%hu", u8s_to_u16(tcp->src_port),
            u8s_to_u16(tcp->dst_port));
}

static void
print_captured_data(u8 const* data, int data_len) {
    if (data_len < sizeof(struct ethernet_frame)) {
        printf("ppf: didn't capture enough to parse ethernet frame. len=%d\n", data_len);
        return;
    }

    struct ethernet_frame const* e = (struct ethernet_frame*)data;
    putchar('\t'); ethernet_frame_print(e); putchar('\n');

    u16 ethertype = u8s_to_u16(e->ethertype);

    if (ethertype == ETHERTYPE_IPV4) {
        if (data_len < sizeof(struct ethernet_frame) + sizeof(struct ipv4_packet)) {
            printf("ppf: didn't capture enough to parse IPv4 header. len=%d\n", data_len);
            return;
        }
        // IPv4
        struct ipv4_packet const* ip4 = (struct ipv4_packet*)(data + sizeof(struct ethernet_frame));
        putchar('\t'); ipv4_packet_print(ip4); putchar('\n');
        
        u8 const* payload = (((u8*)ip4)+ipv4_packet_ihl(ip4)*4);
        putchar('\t');

        switch(ip4->protocol) {
        case IPV4_PROTOCOL_ICMP:
            printf("ICMP");
            break;
        case IPV4_PROTOCOL_IGMP:
            printf("IGMP");
            break;
        case IPV4_PROTOCOL_TCP:
            tcp_packet_print((struct tcp_packet*)payload);
            break;
        case IPV4_PROTOCOL_UDP: 
            udp_packet_print((struct udp_packet*)payload);
            break;
        case IPV4_PROTOCOL_ENCAP:
            printf("ENCAP");
            break;
        case IPV4_PROTOCOL_SCTP:
            printf("SCTP");
            break;
        default:
            printf("Unknown IP protocol %hhu", ip4->protocol);
            break;
        }
        putchar('\n');
    } else if (ethertype == ETHERTYPE_IPV6) {
        // IPv6
        printf("\tIPv6\n");
    } else if (ethertype == ETHERTYPE_ARP) {
        printf("\tARP\n");
    } else {
        printf("\tUnhandled ethertype: %0xX\n", ethertype);
    }
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
    unsigned long counter = 0;
    while(1) {
        rc = pcap_next_ex(pc, &hdr, &data);
        switch(rc) {
        case 1: // packet read
            ++counter;
            printf("packet %lu: caplen=%d, len=%d data=", counter, hdr->caplen, hdr->len);
            print_hex_dump(data, min(hdr->caplen, 20));
            puts("...");
            print_captured_data(data, hdr->caplen);
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

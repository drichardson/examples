#include <mach/mach_types.h>


kern_return_t HelloKernel_start (kmod_info_t * ki, void * d) {
	printf("KEXT has loaded!\n");
    return KERN_SUCCESS;
}


kern_return_t HelloKernel_stop (kmod_info_t * ki, void * d) {
	printf("KEXT will be unloaded\n");
    return KERN_SUCCESS;
}

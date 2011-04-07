#include <stdio.h>
#include "zfor.h"

int main(int argc, char *argv[])
{
	char buf[4096];
	int len;

	if (argc != 3) {
		printf("usage: %s <vhostname> <propname>\n", argv[0]);
		return -1;
	}

	if ((len = zfor_getvconf(argv[1], argv[2], buf, sizeof(buf))) < 0) {
		printf("Failed to get property '%s' for virtual host '%s'!\n",
			   argv[2], argv[1]);
		return -1;
	}

	printf("(%d) %*s\n", len, len, buf);

	return 0;
}

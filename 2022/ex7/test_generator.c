#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>
#include <time.h>
#include <stdbool.h>
#include <assert.h>

static inline uint64_t  rand_atleast(uint64_t  atleast)
{
    uint64_t  result = 0;
    do {
        result = ((uint64_t)RAND_MAX + 1) * result + (uint64_t)rand();
        atleast /= ((uint64_t)RAND_MAX + 1);
    } while (atleast > 0);
    return result;
}

bool in_circle(int64_t x, int64_t y, int64_t r) {
	return (x*x + y*y) <= r*r;
}


int main(int argc, char* argv[]) {
	assert(argc == 3);

	srand(time(NULL));

	int64_t radius = atoi(argv[1]);
	int64_t num_points = atoi(argv[2]);

	printf("%lld\n", radius);
	printf("%lld\n", num_points);

	for (int i = 0; i < num_points; i++) {
		int64_t x, y;
		x = 1000000000;
		y = 1;
		while (!in_circle(x,y,radius)) {
			x = (rand_atleast(radius*2) % (radius*2)) - radius;
			y = (rand_atleast(radius*2) % (radius*2)) - radius;
		}

		printf("%lld %lld\n", x,y);
		
	}
	
}
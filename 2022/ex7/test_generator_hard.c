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

typedef struct {
	int64_t x;
	int64_t y;
} point_t;

int main(int argc, char* argv[]) {
	assert(argc == 4);

	srand(time(NULL));

	int64_t radius = atoi(argv[1]);
	int64_t num_points = atoi(argv[2]);
	int64_t margin = atoi(argv[3]);
	if(margin == -1) {
		margin = radius;
	}

	printf("%lld\n", radius);
	printf("%lld\n", num_points);

	point_t* points = calloc(num_points, sizeof(point_t));

	for (int i = 0; i < num_points; i++) {
		int64_t x, y;
		bool was_chosen_before = true;
		int chosen_loop = 0;

		while (was_chosen_before || !(in_circle(x,y,radius) && !in_circle(x,y, radius - margin))) {
			x = (rand_atleast(radius*2) % (radius*2)) - radius;
			y = (rand_atleast(radius*2) % (radius*2)) - radius;

			was_chosen_before = false;
			for (int j = 0; j < i; j++) {
				was_chosen_before = was_chosen_before || (points[j].x == x && y == points[j].y); 
			}

			chosen_loop++;			
			if(chosen_loop > 100000) {
				fprintf(stderr, "LOOPING Error\n");
				return 1;
			}
			
		}

		points[i].x = x;
		points[i].y = y;
		printf("%lld %lld\n", x,y);
		
	}
	
	free(points);
}
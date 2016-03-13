#include <stdio.h>
#include <stdbool.h>
#include <assert.h>

#define analysis_main main

/******************************************************************************
 * Common helpers
 ******************************************************************************/

/*
   1  2  3
      U
   4 L R 5
      D
   6  7  8

 Figure 1. The Roxor, Inc. In The Groove Dance Game (henceforth "ITG") Dance
 Step Direction Indicators (henceforth "Arrows"), UDLR, and the 8 possible
 directions of facing.
 */

enum facing {
	F_UL, F_U, F_UR,
	F_L,  F_N, F_R,
	F_DL, F_D, F_DR,
	NUM_FACINGS,
};

enum arrow { U, D, L, R, NUM_ARROWS };

enum facing whichway(enum arrow left_foot, enum arrow right_foot) {
	// Foot switches: future work!
	assert(left_foot != right_foot);
	static enum facing way_table[NUM_ARROWS][NUM_ARROWS] = {
		[U][D] = F_R,
		[U][L] = F_DR, // xover
		[U][R] = F_UR,
		//[U][U] = F_U, // footswitch
		[L][U] = F_UL,
		[L][D] = F_UR,
		[L][R] = F_U,
		//[L][L] = F_L, // xover footswitch
		[D][U] = F_L,
		[D][L] = F_DL, // xover
		[D][R] = F_UL,
		//[D][D] = F_U, // footswitch (to be theoretically beautiful, this would be F_D, a backwards switch, but who the hell does that?)
		[R][L] = F_D, // lateral
		[R][U] = F_DL, // xover
		[R][D] = F_DR, // xover
		//[R][R] = F_R, // xover footswitch
	};
	return way_table[left_foot][right_foot];
}

// For any two facings possible with consecutive itg steps (ie, only 1 foot
// moves), shouldn't return more than 2.
// Higher turniness is possible in future work using jumps!
unsigned int turn_distance(enum facing before, enum facing after)
{
	assert(before < NUM_FACINGS && after < NUM_FACINGS);
	static int turn_table[NUM_FACINGS][NUM_FACINGS] = {
		[F_UL] = { 0, 1, 2,
			   1, 0, 3,
			   2, 3, 4, },
		[F_U]  = { 1, 0, 1,
			   2, 0, 2,
			   3, 4, 3, },
		[F_UR] = { 2, 1, 0,
			   3, 0, 1,
			   4, 3, 2, },
		[F_L]  = { 1, 2, 3,
			   0, 0, 4,
			   1, 2, 3, },
		[F_N]  = { 0, 0, 0,
			   0, 0, 0,
			   0, 0, 0, }, // special case facing used when processing jumps - handling jump facing is future work
		[F_R]  = { 3, 2, 1,
			   4, 0, 0,
			   3, 2, 1, },
		[F_DL] = { 2, 3, 4,
			   1, 0, 3,
			   0, 1, 2, },
		[F_D]  = { 3, 4, 3,
			   2, 0, 2,
			   1, 0, 1, },
		[F_DR] = { 4, 3, 2,
			   3, 0, 1,
			   2, 1, 0, },
	};
	return turn_table[before][after];
}

/******************************************************************************
 * Analysis
 ******************************************************************************/

// 1 = tap; 2 = hold; 3 = hold release; 4 = roll; M = mine
// IDK what stepmania does for lifts/fakes; I don't know of any chart that uses them anyway.
#define COULD_BE_NOTE(c) (((c) >= '0' && (c) <= '9') || (c) == 'M' || (c) == 'm')
#define IS_NOTE(c) ((c) == '1' || (c) == '2' || (c) == '4')

bool get_note(char *buf, enum arrow *note)
{
	char l, d, u, r;
	if (sscanf(buf, "%c%c%c%c\n", &l, &d, &u, &r) == 4) {
		if (COULD_BE_NOTE(l) && COULD_BE_NOTE(d) && COULD_BE_NOTE(u) && COULD_BE_NOTE(r)) {
			if (IS_NOTE(l) && !IS_NOTE(d) && !IS_NOTE(u) && !IS_NOTE(r)) {
				*note = L;
				return true;
			} else if (!IS_NOTE(l) && IS_NOTE(d) && !IS_NOTE(u) && !IS_NOTE(r)) {
				*note = D;
				return true;
			} else if (!IS_NOTE(l) && !IS_NOTE(d) && IS_NOTE(u) && !IS_NOTE(r)) {
				*note = U;
				return true;
			} else if (!IS_NOTE(l) && !IS_NOTE(d) && !IS_NOTE(u) && IS_NOTE(r)) {
				*note = R;
				return true;
			}
		}
	}
	return false;
}

void analysis_main()
{
	char buf[256];
	enum facing last_facing;
	enum arrow last_note;
	unsigned int notes = 0;
	unsigned int turniness = 0;
	while (fgets(buf, 256, stdin)) {
		buf[255] = 0;
		enum arrow note;
		// XXX: Ignores jumps, hands, &c.
		if (get_note(buf, &note)) {
			if (notes != 0) {
				// Check for double-step. Foot switches are future work!
				if (note == last_note) {
					continue;
				}
				// XXX: Assumes no doublesteps.
				// NB. Assumes song always starts on left foot.
				// But tbh, it doesn't affect the turniness calculation
				// if we think you're backwards the entire song!
				// XXX: Fails to parse the Boten Anna roll-stream pattern.
				enum facing this_facing = notes % 2 == 0 ?
					whichway(note, last_note) :
					whichway(last_note, note);
				if (notes > 1) {
					// subsequent notes not first or second ones
					unsigned int distance = turn_distance(last_facing, this_facing);
					assert(distance <= 2);
					turniness += distance;
				}
				last_facing = this_facing;
			}
			last_note = note;
			notes++;
		}
	}
	printf("%u turniness (x10000) (%u turns / %u notes)\n", (turniness * 10000 / notes), turniness, notes);
}


/******************************************************************************
 * Synthesis
 ******************************************************************************/

bool is_270(enum facing new_facing, enum facing last_facing, enum facing two_facings_ago) {
	// Facing F_D is allowed, but twisting past that to face the opposite direction you came
	// from isn't.
	if (new_facing == F_DR) {
		if (last_facing == F_DL) {
			return true; // LURD
		} else if (last_facing == F_D && two_facings_ago == F_DL) {
			return true; // LRDLRD
		}
	// symmetric case
	} else if (new_facing == F_DL) {
		if (last_facing == F_DR) {
			return true; // RULD
		} else if (last_facing == F_D && two_facings_ago == F_DR) {
			return true; // RLDRLD
		}
	}
	return false;
}
bool is_spin(enum facing new_facing, enum facing last_facing, enum facing two_facings_ago, enum facing three_facings_ago) {
	// Patterns such as LURD are allowed but you have to twist back around the way you came
	// from. So check at facings L, R, UR, and UL.
	return (new_facing == F_R || new_facing == F_UR ||
		new_facing == F_L || new_facing == F_UL) &&
		is_270(last_facing, two_facings_ago, three_facings_ago);
}

#define SEARCHDEPTH 18
#define MAX(x, y) ({ typeof(x) __x = (x); typeof(y) __y = (y); __x < __y ? __y : __x; })

enum search_rule { NONE = 0, NO_SPINS, NO_270S, NO_LATERALS, NO_XOVERS, NUM_RULES };

void print(enum arrow *chart, unsigned int turniness)
{
	printf("turniness %u: ", turniness);
	for (unsigned int i = 0; i < SEARCHDEPTH; i++) {
		printf("%c", chart[i] == L ? 'L' : chart[i] == D ? 'D' : chart[i] == U ? 'U' : chart[i] == R ? 'R' : '?');
	}
	printf("\n");
}

// returns max turniness reachable
unsigned int search(enum arrow *chart, unsigned int index, unsigned int turniness_so_far, enum search_rule rule)
{
	unsigned int max_turniness = 0;
	if (index == 0 || index == 1) {
		// no previous facing; just do all possible 2-step starts
		for (enum arrow l = 0; l < NUM_ARROWS; l++) {
			for (enum arrow r = 0; r < NUM_ARROWS; r++) {
				if (r == l) continue;
				// avoid illegal starting positions
				if (r == L || l == R) continue;
				// other possible approach
				// if (rule >= NO_LATERALS && whichway(l,r) == F_D) continue;
				// if (rule >= NO_XOVERS && (whichway(l,r) == F_DL || whichway(l,r) == F_DR)) continue;

				// WLOG we say start on left foot
				chart[0] = l;
				chart[1] = r;
				max_turniness = MAX(max_turniness, search(chart, 2, 0, rule));
			}
		}
	} else if (index == SEARCHDEPTH) {
		print(chart, turniness_so_far);
		return turniness_so_far;
	} else {
		// general case
		for (enum arrow next = 0; next < NUM_ARROWS; next++) {
			if (next == chart[index-1]) continue; // footswitches are future work
			// which foot?
			enum facing new_facing;
			enum facing last_facing;
			enum facing two_facings_ago;
			enum facing three_facings_ago;
			if (index % 2 == 0) {
				// About to place a left foot step at chart[index].
				new_facing        =                    whichway(next,           chart[index-1]);
				last_facing       =                    whichway(chart[index-2], chart[index-1]);
				two_facings_ago   = index <= 2 ? F_N : whichway(chart[index-2], chart[index-3]);
				three_facings_ago = index <= 3 ? F_N : whichway(chart[index-4], chart[index-3]);
			} else {
				// Right foot step at chart[index].
				new_facing        =                    whichway(chart[index-1], next);
				last_facing       =                    whichway(chart[index-1], chart[index-2]);
				two_facings_ago   = index <= 2 ? F_N : whichway(chart[index-3], chart[index-2]);
				three_facings_ago = index <= 3 ? F_N : whichway(chart[index-3], chart[index-4]);
			}
			// Consider the sequence of facings. Does the search rule allow it?
			if (rule >= NO_XOVERS) {
				if (new_facing == F_DL || new_facing == F_DR) continue;
			}
			if (rule >= NO_LATERALS) {
				if (new_facing == F_D) continue;
			}
			if (rule >= NO_270S) {
				if (is_270(new_facing, last_facing, two_facings_ago)) continue;
			}
			if (rule >= NO_SPINS) {
				if (is_spin(new_facing, last_facing, two_facings_ago, three_facings_ago)) continue;
			}
			// Got here.
			chart[index] = next;
			unsigned int new_turniness = turniness_so_far + turn_distance(last_facing, new_facing);
			max_turniness = MAX(max_turniness, search(chart, index+1, new_turniness, rule));
		}
	}
	return max_turniness;
}

void synthesis_main()
{
	enum arrow chart[SEARCHDEPTH];
	unsigned int result = search(chart, 0, 0, NONE); // can customize this
	printf("Best turniness %u\n", result);
}

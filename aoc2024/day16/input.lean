def input : String := "\
#############################################################################################################################################
#.......#...#.......#.#.......................#.......#...#...#...#...............#.#.....................#.......#.......#.......#...#....E#
#.#.###.#.#.###.###.#.#.###.#################.#.###.#.#.#.#.#.#.#.#.#######.#####.#.#.#.###.###.#####.#.#.#.#.#####.#.#####.#.###.#.#.#.###.#
#.#.#.#.#.#.....#.#.........................#.#...#.#.#.....#...#.........#.#.....#...#.#.#...#.#.#...#.#...#.#...#.#.#.....#.#.#.#.#.....#.#
###.#.#.#.#####.#.#.#.#####.#####.#.###.#.#.#.#.###.#.#####.###########.###.###.#######.#.###.#.#.#.###.#.#.#.#.#.#.#.#.#####.#.#.#########.#
#...#.#...#.....#...#.#.....#.....#...#.#.#.#...#...#.....#...#...#.......#...#.........#...#.#.#.....#.......................#.#.........#.#
#.###.#####.#####.###.#.###.#.###.#.#.#.#.#.#####.#######.#.###.#.#.###.#.###.###########.###.#.#####.###.#.#.#######.#.###.###.#########.#.#
#...#...#.......#...#...#...#.#.....#.#.#.#.......#.....#...#...#.#.#...#...#.....#...#.....#.#.....#.#...#.#.#.....#...#...#.....#.....#.#.#
#.#.#.#.#.#####.###.#.###.#.#.#####.#.###.###.#########.###.#.###.#.#.#.###.#####.###.#.###.#.#####.#.#.#.#.#.#.###.#.###.###.###.#.###.#.#.#
#.#.#.#.#.#...#.....#...#.#.#.#...#.#.#...#...#.......#...#.#.#.....#.....#.......#...#...#...#.....#.#.#.....#.#.#.#.#...#...#...#.#.#...#.#
#.#.###.#.#.#######.#####.#.#.#.#.#.#.#.###.###.#####.#.#.#.#.#########.#.#########.#####.#####.#####.#####.#.#.#.#.#.#.###.#.#####.#.#####.#
#.#...#...#.#.....#.....#.#.#...#.#.#.#...#...#.#...#.#.#.#...#...#...#...#.......#.....#.#.....#...#...#.......#...#.#.#...#...#...#...#...#
#.###.#.###.#.###.#####.#.#.#####.###.###.###.#.###.#.#.###.#.#.#.#.#.#.#.#.#####.#####.#.#.###.#.#####.#.#.#.###.#####.###.###.#.###.#.#.#.#
#...#.#.....#...#...#...#.#.#...#...#...#.#...#...#.#.#.#...............................#.#.#.#.#.........#...........................#...#.#
#.###.#####.#.#####.#.###.#.#.#.###.#.#.#.#######.#.#.#.#.#####.#.#.###.#####.###.#.#.###.#.#.#.#.#########.#.#######.#.#.#################.#
#.#...#.....#.#.....#...#.#.#.#...#...#.#...#.....#.#.#...#.....#.#.....#...#.#.....#.#...#...#.#.#.......#.........#.#.#.#.......#.....#...#
#.#.###.#####.#.#.#####.#.#.#.#.#######.###.#.#####.#.#####.#.###.#####.#.#.#.#.#######.#######.#.#.#####.#####.#####.###.#.#####.#.#.###.###
#.#...#...#...#.#.#...#...#.#.#.#.......#.#.#.#.....#...........#.#.......#.#...#.....#.#.......#.#.....#.#.....#.......#.#.#...#.#.........#
#.###.#####.###.###.#.#######.#.#.#####.#.#.#.###.###########.###.#.###.#.#.#####.#.#.#.#.#.###########.#.#.#.###.#####.#.#.#.###.#.#.#.###.#
#.#.#.......#.#.#...#.....#...#.#.#...#...#.#...#...#.....#...#.....#.....#.#.....#.#...#.#.#.............#.#.#...#...#.#.#.#.#...#.#.#...#.#
#.#.#########.#.#.#######.#.###.#.#.#.###.#.###.###.#.###.#.###.###.#.#####.#.#.###.#####.#.#.###########.#.#.#.###.#.#.#.#.#.#.###.#.###.#.#
#...#.............#.....#.#...#...#.#...#.#...#.....#.#...#...#.#...#.#.....#...#.......#.#.#...#.....#...#.........#.#...#...#.#.......#.#.#
###.#.###.#.#.#####.#.###.###.#####.#.#.#####.###.###.#.#.###.#.###.#.#.#######.#####.#.###.###.#####.#.#########.#.#.#######.#.###.#.###.#.#
#.#...............#.#.#.....#.#...#.#.......#.....#...#.....#.#...#.#.#.......#.......#...#...#.......#...........#.#.#.......#.....#.....#.#
#.#.#.#.#.###.###.###.#.###.#.#.#.#.#######.#.#####.#######.#####.#.#.#######.###########.#.#.#######.###########.#.#.#.###########.#.#.###.#
#...#.#.#.....#.#...#.#...#.#...#.#...#...#.#.....#.....#.#.#.....#.#.......#...#.......#.#.#.#.....#.......#...#...#...#.........#...#...#.#
#.#####.#####.#.###.#.###.#####.#####.#.###.#.#.#.#####.#.#.#.#####.###.#.#.###.#.#####.#.#.#.#####.#####.#.#.#.#.#######.###.###.#.#.###.#.#
#.................#.....#.......#.....#...#.#.#.#...#...#.......#.....#.#.#.#.#...#...#.#.#.#.......#...#.....#.......#...#...#...#.#.#.....#
#.###.###.#.#.###.#####.#########.#####.#.#.###.###.#.###.#####.###.#.#.#.#.#.#######.#.#.#.#######.###.#.###.#.#####.#####.###.###.#.#####.#
#.#...#...#.#...#.#.#...#.#.......#...#.#.#.....#...#...#...........#.#...#.#.......#.#...#.#.#.....#...#...#.#.#...#.......#...#...#...#...#
#.#.###.###.#.#.#.#.#.###.#.#######.###.#.#######.#####.#####.#.#.#.#######.#####.#.#.#####.#.#.#####.#####.###.#.#.#########.#####.###.###.#
#...#.#...#.#.#.#.#.#...#.#.#.......#...#.#.............#.......#...#.....#.......#.......#.#...#.........#.....#.#.....#.....#.....#.#...#.#
#####.#.###.#.#.#.#.###.#.#.###.###.#.###.#.#####################.###.###.#######.#######.#.#.#########.#.#######.#####.#.#####.###.#.###.#.#
#.#...#.#...#.#.#.#...#...#...#...#.#.#.#...#.....#.............#...#...#.#.......#...#.....#.........#.#.......#...#.#.#...#...#.#.#...#.#.#
#.#.#.#.#.###.###.#.#.#.#####.###.#.#.#.#####.#.#.#.#####.#####.#.#.###.#.#.#####.#.#.###############.###.###.#.###.#.#.###.#.###.#.###.#.#.#
#.#.#.#.#.#.......#.#.#.#.....#...#.#...#.......#.#.......#...#.#.......#.#.......#.#...........#.......#.#...#...#.#.#.....#.#.......#.#.#.#
#.#.#.#.#.###.#.#####.#.#.#####.###.#.#.#.#####.#.#######.#.#.#.#.#######.#######.#.###########.#.#.###.###.#######.#.#######.#######.#.#.#.#
#.#.#.#.#.....#.......#.#.#.....#.....#.#.....#.#.......#.#.#.#.#.......#.#.......#.#.......#.#.#.#...#.#...#.......#.......#.......#.#.#.#.#
#.#.#.#.#####.#######.#.#.#####.#######.#.#.#.#######.###.#.###.#########.#.#######.#.#####.#.#.###.#.#.#.#.#.#######.#.###.#.#####.#.#.#.#.#
#...#...#.....#.....#...#.#...#.#...#...#.#.#.........#...#...#.#.....#.....#.......#.....#...#.....#.#...#.#.....#...#.#...#...#...#...#.#.#
#.#######.###.#.###.###.#.#.#.#.#.#.#.#####.#####.#####.#####.#.#.###.#.###.#.###########.###.#######.#.#######.#.###.#.#.###.###.#####.#.###
#.#.....#.......#.#.#...#.#.#.#...#.........#...#.#...........#...#...#.....#.....#.......#.#...#...#.#.#.......#...#.#.#...#.#...#.....#...#
#.###.#.#####.###.#.#.###.#.#.#########.#####.#.###.#.###.###.#####.#####.#######.#.#######.###.#.#.#.#.#.#.###.###.#.#.###.#.#.###.#######.#
#...#.#.....#.....#...#.#...........#.#.#...#.#.....#...#.#...#...#.....#...#.....#.#.#.........#.#...#.#.#...#.....#.#...#.#...#.....#.....#
###.###.###.#.#.#######.###########.#.#.#.#.#.#######.#.#.#####.#.#####.#####.#####.#.#.#########.#.#.###.###.#.#.#.###.#.#.#.###.#####.###.#
#.#.....#.#.#.#.......#.....#.....#.#...#.#.#.........#.#.....#.#.....#...#...#.....#.#.#.....#...#.....#.#.#...#.#...#.#.#.#.#.#.#.....#...#
#.#######.#.#####.#.#.###.#.###.###.#.###.#.###.#.###.#.#####.#.#####.###.#.###.#.###.#.#.###.#.###.#.#.#.#.###.#####.#.#.#.#.#.#.#.#####.###
#.......#.#.....#.#...#...#.....#...#...#.#.....#...#.#.......#...#.#...#.#.#...#...#.#.#.#...#...#.#.#.#...#.#.......#.#.#...#...#.#...#.#.#
###.###.#.#####.#.#.###.#########.#######.#####.###.#.#######.###.#.###.#.#.#.#####.#.#.#.#.###.#.#.#.#.###.#.#########.#.###.#####.#.#.#.#.#
#...#.#.#.....#...#...#.#...#...#.......#.#.......#...#...#...#.#.#.....#.#...#...#.#.....#.#...#.#.#.#.#...#.........#.#.#...#.....#.#...#.#
#.###.#.#.###########.#.#.#.#.#.#######.#.#.###########.###.###.#.###.###.#.###.#.#.#######.#.###.#.#.#.#.###.#######.###.#.###.#####.###.#.#
#.#...#.#.........#...#.#.#.#.#.......#.#.#.......#...#.....#...#...#...#.#.....#.#...#...#...#...#.#.#...#...#...#...#...#.#.#.#...#...#...#
#.###.#.#.#######.#.###.#.#.#.#######.#.#.#######.#.#.#.#####.#####.###.#.#######.#.#.#.#####.#.#.###.#.#.#.###.#.#.###.###.#.#.###.#.#.#.###
#...............#.#...#...#.#...#...#...#.#.....#.#.#...#.....#...#.#...#.......#.#.....#.....#.#.#...#...#.#.#.#.......#.#...#...#...#.#...#
#.#.###.#.#####.#.###.#####.###.#.#.#.###.#.###.#.#.#####.#####.#.#.#.###.#####.#.###.#.#.#####.###.#######.#.#.#########.###.###.###.#.#.#.#
#.#.....#.......#...#.....#...#...#.#.....#.#...#...#.....#.....#...#.....#.#...#...#.#.#.#...#...#.#.......#.....#...#.........#...#.#.#...#
#.#.#######.#######.#####.#.#.#####.#.#####.#.#######.###.#.###############.#.#####.#.#.#.#.#.#.#.#.#.#.###########.#.#.###.#######.#.#.###.#
#.#...#...............#.....#.....#.#.#.....#.....#...#...#.#.....#.....#...#.#.#...#.#.#.#.#...#...#.#.#.....#.....#.#.....#.....#.#.....#.#
#.###.#.###.#.#########.#######.#.#.#.###.#######.#.#.###.#.#.###.#.#####.#.#.#.#.###.#.#.#.#######.###.#.###.#.###.#.#######.###.#.#.###.#.#
#...#.#.#...#.#.....#...#.#.....#...#...#.#...#.#...#...#.#.#...#.#.......#.....#.#...#...#.............#.#.#.#.#.............#.#.#...#.....#
###.#.#.#.#.#.#.###.#.###.#.#######.###.#.#.#.#.#######.###.###.#.#####.#########.###.#.###.#######.#####.#.#.#.###############.#.#.#.#####.#
#...#.#.#.#.#.#.#.#...#...#.#...........#.#.#.....#...#...#.....#.#...#...#...........#...#...#.....#.....#.#.#.........#.......#.#.#.#...#.#
#.###.#.#.#.#.#.#.#####.#.#.#.###########.#.#####.#.#.###.#######.#.#.#####.#.###########.###.#.#.###.#####.#.#########.#####.###.###.#.#.###
#.............#.......#...#...#.......#.#.#.....#...#...#.....#...#.#.........#...#.....#.#...#.#.#...#.....#.#.......#.....#...#...#...#...#
#.#########.#########.#.#######.#####.#.#.#####.#####.#######.#.###.#########.#.#.#####.#.#####.###.###.###.#.#.#####.###.#.#.#.###.#.#####.#
#.#...............#...#...#.....#.......#.....#.............#.#...#.#.#.....#.#.#.....#.........#...#...#.#.#.#...#.....#.#.#.#.....#.#.....#
#.#.#######.###.###.#####.#.###.#############.###.#######.#.#.###.#.#.#.###.#.#.#####.#######.###.###.###.#.#.#.#.#####.#.#.#.#########.#####
#.#.#.........#...#.....#...#.#.#...........#...#.....#.....#...#.#.#.#.#.#.#.#...#.....#.....#...#.#.#...#...#.#.#...#.#.#.#.....#.....#...#
#.#.###.#.#######.#####.#.###.#.#.#########.###.#####.#.###.###.#.#.#.#.#.#.###.#.#####.#####.#.###.#.###.#####.#.#.#.#.###.#####.#.#####.#.#
#.#...#.#.......#.............#.......#...#.....#.....#.#.#...#.#...#.#.#.#...#.#.....#.....#.#.#...#.........#.#...#.#...#...#.#.#...#...#.#
#.###.#.#####.#.#####.#######.#######.###.#########.###.#.#.###.#####.#.#.###.#.#####.#####.###.#.###########.#.#####.###.###.#.#.#.#.#.#####
#.....#.#...#.......#.....#.....#...#.....#.......#.#...#...#.........#.#.#...#.#.....#...#.....#...........#.......#.#.#.........#...#.....#
#.#######.#.#####.#.###.#.#.#####.#.#####.#.#####.###.###.#.#.#######.###.#.#####.#####.#########.#####.#.#.#######.#.#.#######.###.###.###.#
#.#.......#.......#.....#.......#.#.......#.#...#...#...#...#.........#...#.......#...#.......#.....#.....#.#.....#.#.#.............#.#.#.#.#
#.#.#######.###################.#.#.###.#.#.###.###.###.#########.#.#.#.#.#########.#.#.#.###.#.#####.#####.#.#.#.#.#.#.#.###########.#.#.#.#
#.#.......#.#.........#.......#...#...#.#...#...#.......#.........#...#...........#.#.#.#.#.....#.#...#.....#.#.#.#.....#.............#...#.#
#.###.###.###.#######.#.#############.#.#####.###.#######.#########.#.#.#.#######.###.#.#.###.#.#.#.###.#####.#.#.#####.###########.#.###.#.#
#...#...#.#...#...#...#.....#.......#.#...........#.....#.#.........#...#.#.....#...#.#.#...#.#.#.#...#.#.....#.#...#...#.....#...#.#...#.#.#
###.#####.#.#####.#.###.###.#.#####.#.###.#.#######.###.#.#.#############.#.###.###.#.#.#.#.###.#.###.###.#####.#####.#####.###.#.#.#.###.#.#
#.#.....#.#.....#.#...#...#.#.#.....#...#...#.......#.#.#.#.....#.......#.....#.#...#.#.#.#.....#...#...#...#.#.............#...#.#.#.#...#.#
#.#####.#.#.#.#.#.###.#####.#.#.#######.#.###.###.#.#.#.#.#####.#.#.###.#.###.#.###.#.#.#.#######.#.###.###.#.#######.#######.###.###.#.###.#
#.......#.#...#.....#.#...#...#.#...#...#...#.#.....#.....#...#.#.#...#...#.#.#...#...#.#.#.......#.#.#...#.#.......#...#...#.........#...#.#
#.#######.#########.#.#.#.###.#.#.#.#.###.###.#############.#.#.#.###.###.#.#.###.#.###.#.#.###.###.#.###.#.#.###.###.#.#.#.#.#.#########.#.#
#...#...............#...#...#.#...#.#.....#...#...........#.#...#...#...#.#.#.#.#.#.#...#...#...#.#.....#...#...#.#...#...#...#.#.........#.#
###.###############.#######.#.#####.#.#.###.###.#######.#.###.#########.#.#.#.#.#.###.###.#.#.###.#####.###.###.#.#.#####.#######.#####.###.#
#.#...............#.#.....#.#.#.....#.#...#.....#.#...#.#...#.....#...#.#...#.#.#.....#.#.#.#...........#.......#.#.#.....#.......#...#.#...#
#.###############.#.#.###.#.#.#.#####.###.###.###.#.#.#####.#####.#.#.#.###.#.#.#######.#.#.#.#.#.#####.###.#####.#.#######.#######.#.#.#.#.#
#.......#...#.....#.#.#...#.#.#.....#.#.#.#.......#.#.......#...#...#.#...#.#.#.....#...#...#.#...#...#...#...#...#...#.......#...#.#.....#.#
#.#####.#.#.#.###.###.#.###.#.#####.#.#.#.#.###.###.#########.#######.#.#.###.#.###.#.#.###.#.###.#.#.###.#.#.#######.#.#######.#.#.#########
#.....#.#.#...#.......#.#...#.......#.....#.#...#...#...............#.#.....#.#...#...#...#.#.#...#.#.#...#.#.#.......#.#.....#.#.#.........#
#.###.###.#####.###.###.#.#######.#.#.#.#####.###.#########.#####.#.#.#####.#.###.#######.#.#.#.###.#.###.###.#.#.#.###.#.###.#.#.#########.#
#...#.....#.......#...#.#.#.......#.#.#.....#...#.#.......#.....#.#.#.#...#.....#...#.......#.#.#...#...#.#...#.#...#...#...#.#.#.#.........#
###.#################.#.#.#.#######.#.#####.###.#.#.###.#.#####.#.###.#.#.#.###.###.#.#.#.###.#.#.#####.#.#.#.#.#.###.#####.#.#.#.#.#######.#
#...#.............#...#...#.#.....#.#...#.............#...#.....#.#...#.#.#...#.....#.#.......#.#.#.....#.#.#.#.#.#...#...#.#.#.#.#.#...#...#
#.###.###########.#.#######.#.###.#####.#######.#####.#.#########.#.###.#.#.#.#.#####.#####.#####.#.###.#.#.###.#.#.#####.#.#.#.#.#.#.#.#.###
#.#.........#...#.#.#...........#.....#.......#.....#.#...#...#...#...#.#...#.#...#...#.....#...#.#.#...#...#...#...#.....#.#...#...#.......#
#.#########.#.#.#.#.#.###############.###.###.###.#.#####.#.#.#.#####.#.#.###.###.#.###.#####.#.#.#.#.#######.###.#.#.#####.#.#####.#######.#
#...#.......#.#.#...#.......#.........#.#.....#...........#.#.#.#.....#.#.#...#.#...#...#...#.#.......#.......#...#.........#.#...#.#...#.#.#
#.#.#.#######.#.###.#####.#.#.#######.#.#.#####.###########.#.#.#.#####.#.#.###.#####.#.#.###.#####.###.#############.#.#####.#.#.###.#.#.#.#
#...#...#.....#...#.......#...#.#.....#.#...#...#...#.......#...#.#.....#.#.....#...#...#.#...#...#.#...............#.#.#.......#.#...#.#...#
#.#####.#.#####.#.#.###########.#.#####.###.#.###.#.#.#.###.#####.#.#####.###.#.#.#.#.###.#.###.###.#.#############.#.#.#.#######.#.###.#.###
#.......#.....#.#...#.#.........#.........#.#.#...#.#.#.#...#.....#.....#.....#...#.#.....#.#.......#.........#...#...#.#.......#...#...#.#.#
###########.###.#####.#.#######.#########.#.#.#####.#.#.###.#.#####.###.#.#########.#####.#.#####.#######.###.###.#####.#######.#####.###.#.#
#.....#.......#...#.....#.#...#.....#...#.#.........#.#.....#...#...#...#...#.............#.....#.#...#.....#.....#.....#.............#.#...#
###.#.###.###.#.#.###.###.#.#.###.#.#.###.#####.#####.#####.#.#.#.###.#####.#.###.#.#####.#####.#.#.#.#.###.#####.#.###.###.#.#.#.#.#.#.###.#
#...#.....#.#.#.#...#...#.#.#.....#...#...#.....#.....#...#...#.#...#.....#...#.....#.....#...#.#...#.#.#.........#...#...............#...#.#
#.#########.#.#####.###.#.#.###########.#####.#.#.#####.###.#.#.###.#####.#.#########.#####.#.#.#####.#.#####.#######.#####.###.#.#.#.#.###.#
#.........#.#.......#.#.#.#.....#...............#.#.....#...#.#...#...#.#.#.#...............#...#...#.......#.#...#...#...#.#...#.#.#.#...#.#
#.#######.#.#########.#.#.#####.#.#############.#.#.#.###.#######.###.#.#.###.#############.#####.###.#.###.#.#.#.#.###.###.#.#####.#.###.#.#
#.#.#.....#...#.........#...#...#.#...#.....#...#.#.#...#.......#...#...#...........#.....#.#.....#...#.#.#.#...#.#.#...#...#.......#...#.#.#
#.#.#.#####.#.#.#######.###.#.###.###.#.###.#####.#.#.#.#######.#.#.###.###########.#.###.#.#.###.#.###.#.#.#####.#.#.###.#####.#######.#.#.#
#...#.#...#.#...#.....#...#.#.....#...#.#.#...#...#...#...#...#...#...#.#.........#.#.#.#...#.#.#.#.#...#.#.....#...#.#...#.....#.......#.#.#
###.#.#.#.#######.###.###.#.#######.#.#.#.#.#.#.#######.#.#.#.###.###.#.#.#######.###.#.#####.#.#.#.###.#.#####.#####.#.###.#####.#.#.###.#.#
#...#.#.#...#.....#...#...#.......#.#.#...#.#...#.......#.#.....#.....#.#.#...#...#...#.....#.#...#...#.#.....#...#.....#...#.....#.#.#.....#
#.###.#.###.#.#####.#####.#.#####.#.#.###.#.#####.#######.#####.#######.#.###.#.###.###.#.#.#.#.#####.#.###.#.###.#.#####.###.#####.#.#.#####
#...#.#.#.........#.#...#...#.#...#.#...#.........#.#.....#.........#...#...#.......#...#.#...#...#...#...#.#...#.#.#.....#.#...#.#.#.....#.#
#.#.#.#.#.#####.#.#.#.#.#####.#.###.###.#####.#####.#.#####.#######.#####.#.#############.#####.###.#####.###.#.#.###.#####.###.#.#.#.###.#.#
#...#.#.......#.#...#.#.#.....#...........#.....#...#...#.#.....#.#.....#.#.....#.....#...#...#.#...#...#.....#.#.#...#.......#.#...#.#.#...#
###.#.#####.###.#####.#.#.#####.#####.#.###.#.###.#.###.#.#.###.#.#####.#.#####.#.###.#.###.###.#.###.#.#####.###.#.#######.###.#.###.#.###.#
#...#.#.....#...#.....#.#.............#.#...#.....#.#...#...#.#.#.....#.#.#.......#...#...#.....#.....#...#...#...#.#.......#...#.#...#.#...#
#.###.#.#####.###.#####.#.#############.#.#########.#.#####.#.#.###.#.#.###.#.###.#.#####.#####.#########.#####.#.#.#.#.###.#.#.#.#.###.#.###
#.#...#.#...#.........#.#.....#.....#...#.....#.....#.....#...#.#...#.#.#...#...#.#.#.....#...#.#.......#.....#.......#.....#.#.#...#.#...#.#
#.#.###.#.#.###########.#####.#.###.#.#######.#.#########.#####.#.#####.#.#####.###.#.#####.#.###.#.#.#######.#########.#####.#.#####.#.###.#
#.#.........#.........#...#...#...#.#.......#.#.......#...#.....#...#...#.#.........#.....#.#.....#.#.#.....#.....#.........#.#.......#.#...#
#.###########.#######.###.#######.#.#########.#####.#.#.###.#######.#.#.#.###########.###.#.#######.###.###.#####.#######.###.#.###.###.#.#.#
#.....#.......#...#...........#...#.#...#...#...#.#.#.#.#...#.......#.#.......#.#.....#.#.#...#...#.....#.#.......#.....#.#...#...#.#...#.#.#
#####.#.#.#.###.#.#########.#.#.###.#.#.#.#.#.#.#.#.###.#.#.###.#.#.#.#.#####.#.#.#.#.#.#.###.#.#########.#########.###.###.###.#.#.#.###.#.#
#.#...#.#.#...#.#...#.....#.#.#.#...#.#...#...#.#.#.#...#.#.....#.#...#.....#.#.........#.....#.......#.............#...#...#...#.#.#...#.#.#
#.#.###.###.#.#.#.#.#.###.#.#.#.#.###.#.#####.#.#.#.#.#.#.#.###.###.#.#####.#.#####.#.#.#########.###.#####.#######.###.#.###.#.#.#.###.#.#.#
#...#.#.....#...#...#.#...#.#...#...#.#...#...#.#.#...#...#...#.#...#.......#...#...#...#.......#.#.......#...#...#...................#.#.#.#
#.###.###########.#.#.#.###.#####.#.#.###.#.#.#.#.#####.#.#.#.###.#.#.#########.###.#.#######.#.#.#######.#.###.#.###.#.#.#.###.#.#####.###.#
#.#.............#.#.#.#.....#.....#.#...#.#.#.#.#.......#.#.#.....#.#.........#...#.#.....#...#.#.....#...#.#...#...#.#...#.#.....#...#.#...#
#.#.#.#####.#####.#.#.#######.###.#.###.#.#.#.#.#.#.###.#.#.#######.#.###########.#.#.#.#.#.###.#####.#.#####.###.#.#.#####.#.#####.#.#.#.###
#.#.#.#.....#.....#.#.......#.....#...#...#.....#.#.#.#...#...#...#...#.....#...#.#.....#...#...#...#.#.#...........................#.......#
#.###.#.#####.#.###########.#####.###.###.###.#####.#.#.#####.#.#.#.###.###.#.#.#.#.#.#.#####.###.#.#.#.#.#####.#.#.#.#.#.#.#.#.#.#########.#
#.....#.#...#.#.#...#.............#.....#.....#.....#.......#.#.#.....#...#...#.#...#.#...#...#...#...#.#...#...#.#...#.#...#.#.#...........#
#######.###.#.###.#.#.###.###.###.#.#########.###.#####.#.###.#.###.#.#.#.#####.###.#.###.#.###.#######.#.#.#.#.#######.#####.#.###.#.#####.#
#.....#.#...#...#.#.#.#.#.....#.....#.......#...#.......#.#...#.....#.#.#.#...#.....#...#...#...#.#...#.#.#.#.#...#.....#...#.#...#.#.#...#.#
#.###.#.#.#.###.#.#.#.#.###.#.#.#.###.###.#####.#######.#.#.###.#.#.#.###.#.#######.#.#.#######.#.#.#.#.###.#.###.#.#####.#.#.###.###.#.###.#
#S..#.....#...#...#.......#...#.........#...............#.......#.#.............................#...#.......#...#.........#.#.........#.....#
#############################################################################################################################################"

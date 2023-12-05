#!/usr/bin/env perl

use diagnostics;
use warnings;
use strict;

use List::Util qw/reduce/;
use Data::Dumper;

my @lines = (<>);

my $line_length = length($lines[0]);
warn("found $#lines lines");

# possible count for each cube type
my %cubes = (red => 12, green => 13, blue => 14);

my @games = ();

foreach (@lines) {
    next unless m/^\s*Game\s*(\d+):(.+)$/;
    my $id = $1;
    my @rounds = split(/;/, $2);

    # We're using eval to break out of these horrible nested loops
    eval {
        foreach (@rounds) {
            my @reveals = split(/,/, $_);
            foreach (@reveals) {
                next unless m/(\d+)\s*([a-z]{3,5})/;
                my $count = $1;
                my $color = $2;
                my $limit = $cubes{$color};

                die "invalid game!" if $count > $limit;
            }
        }
        push(@games, $id);
    } or do {
        warn("we found an invalid game: $@");
    }
}

warn "found the following possible games: ", Dumper @games;

# Huh, apparently
my $total = reduce { $a + $b } @games;

print "total of fair game ids is $total\n";

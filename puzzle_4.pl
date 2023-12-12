#!/usr/bin/env perl

use diagnostics;
use warnings;
use strict;

use List::Util qw/max min reduce/;
use Data::Dumper;

my @lines = (<>);

my @powers = ();

foreach (@lines) {
    next unless m/^\s*Game\s*(\d+):(.+)$/;
    my $id = $1;
    my @rounds = split(/;/, $2);

    my @all_counts = map {
        my %counts = ();
        my @reveals = split(/,/, $_);
        foreach (@reveals) {
            next unless m/(\d+)\s*([a-z]{3,5})/;
            my $count = $1;
            my $color = $2;
            $counts{$color} = $count;
        }
        \%counts;
    } @rounds;

    my @maxes = map {
        my $color = $_;
        my $max = max map { $_->{$color} or 0 } @all_counts;
        { color => $color, min => $max }
    } qw/red green blue/;
    warn "found the following minimum cube count, Game ${id} ", Dumper @maxes;
    
    my $power = reduce { $a * $b } map { $_->{min}} grep { $_->{min} } @maxes;
    warn "power for Game ${id}: ${power}";

    push(@powers, $power);
}

my $total = reduce { $a + $b } @powers;

print "total of fair game ids is $total\n";

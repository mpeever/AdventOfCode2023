#!/usr/bin/env perl

use diagnostics;
use warnings;
use strict;

use List::Util qw/reduce max/;
use Data::Dumper;

# Sluuurp!
my @lines = (<>);
chomp @lines;

my @winnings = ();

foreach (@lines) {
    next unless m/^([^:]+):([^\|]+)\|(.+)$/xo; 

    my ($label, $winning_numbers, $numbers) = ($1, $2, $3);

    warn "Found label ${label}, winning numbers ${winning_numbers}, numbers ${numbers}";

    my $winnings = 0;
    while ($winning_numbers =~ m/(\d+)/gxo) {
	my $winner = $1;
	
	if($numbers =~ m/\b($winner)\b/) {
	    $winnings = $winnings == 0 ? 1 : $winnings * 2;

	}
    }
    push (@winnings, $winnings);
    warn "winnings: ${winnings}";
}

warn "card scores: ", Dumper \@winnings;

my $sum = reduce { $a + $b } @winnings;

print "Sum of winnings: ${sum}\n";

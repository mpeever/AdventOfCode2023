#!/usr/bin/env perl

use diagnostics;
use warnings;
use strict;

use List::Util qw/reduce max/;
use Data::Dumper;

# Sluuurp!
my @lines = (<>);
chomp @lines;

my %cards = ();
my @winnings = ();

foreach (@lines) {
    next unless m/^(Card\s+(\d+)):([^\|]+)\|(.+)$/xo; 

    my ($label, $card_id, $winning_numbers, $numbers) = ($1, $2, $3, $4);

    unless (defined $cards{$card_id}) {
	$cards{$card_id} = { label => $label,
			     winning_numbers => $winning_numbers,
			     numbers => $numbers,
			     instances => 1 }
    }
    else {
	$cards{$card_id}->{instances}++;
	$cards{$card_id}->{label} = $label;
	$cards{$card_id}->{winning_numbers} = $winning_numbers;
	$cards{$card_id}->{numbers} = $numbers;
    }
    
    my $matches = 0;
    while ($winning_numbers =~ m/(\d+)/gxo) {
	my $winner = $1;
	$matches++ if $numbers =~ m/\b($winner)\b/;
    }
    my $card = $cards{$card_id};
    
    for (my $i = 0; $i < $card->{instances}; $i++) {
	for (my $j = 1; $j <= $matches; $j++) {
	    my $next_card_id = $card_id + $j;
	    
	    $cards{$next_card_id} = { instances => 1 }
	    and next
		unless defined $cards{$next_card_id};
	    my $instances = $cards{$next_card_id}->{instances};
	    $cards{$next_card_id}->{instances} = ++$instances;
	}
    }
    
    warn "card_id: ${card_id}, matches: ${matches}";
}

warn "cards: ", Dumper \%cards;

my $sum = reduce {
    $a + $b
} map {
    $cards{$_}->{instances}
} (keys %cards);

print "Final tally of cards: ${sum}\n";

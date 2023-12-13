#!/usr/bin/env perl

use diagnostics;
use warnings;
use strict;

use List::Util qw/reduce max/;
use Data::Dumper;

# Sluuurp!
my @lines = (<>);
chomp @lines;

my @gears = ();
my $gear_id = 0;
for (my $i = 0; $i <= $#lines; $i++) {
    my @line_gears = ();
    while($lines[$i] =~ m/(\*)/g) {
        # a gear is a single character, it doesn't have a start and end, just a row and column.
        my $start = $-[1];
        push (@line_gears, {id => $gear_id++, line => $i, col => $start});
    }
    push(@gears, @line_gears);
}

my %gear_ratios = ();

for (my $i = 0; $i <= $#lines; $i++) {
    # skip this line if there aren't any gears on it, above it, or below it
    my @near_gears = grep {
	my $gear = $_;

	$gear->{line} == $i
	    or ($i > 0 and $gear->{line} == $i - 1)
	    or ($i <= $#lines and $gear->{line} == $i + 1) 
    } @gears;
    
    warn "skipping line ${i}, because it's not near a gear"
	and next unless scalar @near_gears;

    # find all the numbers on the line
    while ($lines[$i] =~ m/(\d+)/g) {
        my $value = $1; 

        my $start = $-[1];
        my $end = $+[1] - 1;
        my $len = $+[1] - $-[1]; 

        # BEWARE! negative substr indices in Perl count from the RIGHT
        my $strstart = max(0, $start - 1);   # start one character to the left of our number
        my $strlen = $start == 0 ? $len + 1 : $len + 2 ;     # one character to the left, one to the right of our number

        # if this number doesn't touch a gear, bail on it
        warn "skipping ${value} at line ${i}, col ${start} because it's not near a gear" 
	    and next unless ($i > 0 and substr($lines[$i - 1], $strstart, $strlen) =~ m/\*/o)
	    or substr($lines[$i], $strstart, $strlen) =~ m/\*/o
	    or ($i < $#lines and substr($lines[$i + 1], $strstart, $strlen) =~ m/\*/o);
        
        my @touching = grep { 
            my $gear = $_;

            # Any gear with an X- coordinate on [$strstart, $strstart + $strlen] touches our number
            # because it's either on the same line, one above it, or one below it.
            $gear->{col} >= $strstart and $gear->{col} < $strstart + $strlen;
        } @near_gears;

        warn "no gear touches" 
	    and next unless scalar @touching;
	foreach my $gear (@touching) {
	    push(@{$gear_ratios{$gear->{id}}->{numbers}}, $value)
		and next
		if (defined $gear_ratios{$gear->{id}});
	    
	    $gear_ratios{$gear->{id}} = {gear => $gear, numbers => [$value]};
	}
    }
}

foreach my $key (sort keys %gear_ratios) {
    # remove gears without exactly two numeric neighbors
    next if scalar @{$gear_ratios{$key}->{numbers}} == 2;

    my $gear = $gear_ratios{$key}->{gear};
    
    warn "removing gear ratio: ", Dumper $gear_ratios{$key};
    
    my $col = max(0, $gear->{col} - 2);
    warn substr($lines[$gear->{line} - 1], $col, 5) unless $gear->{line} == 0;
    warn substr($lines[$gear->{line}],     $col, 5);
    warn substr($lines[$gear->{line} + 1], $col, 5) unless $gear->{line} >= $#lines;

    delete($gear_ratios{$key});
}

warn "found pruned gear ratios: ", Dumper \%gear_ratios;

my @ratios = map {
    my $id = $_;
    reduce {$a * $b} @{$gear_ratios{$id}->{numbers}};
} (sort keys %gear_ratios);

warn "gear ratio products: ", Dumper \@ratios;

my $sum = reduce { $a + $b } @ratios;

print "Sum of gear ratios: ${sum}\n";

#!/usr/bin/perl

my $tot_samples = @ARGV ? $ARGV[0] : 0;
my $nsamples = 0;
my @legal_mults;
my $sum_mults = 0;
while ($line = <STDIN>) {
  @a = split / /, $line;
  if (@a <= 6) { print "not in form FEN data: $line\n"; exit(1); }
  if ($a[6] !~ /legal/) { print "no legality: $line\n"; exit(1); }
  $mult = $a[5];
  $sum_mults += $mult;
  if ($a[6] !~ /illegal/) { $legal_mults[$mult]++ };
  $nsamples++;
}
my $avg_recip_mult = 0;
my $nlegal = 0;
for my $i (1..$#legal_mults) {
  $nlegal += $legal_mults[$i] ;
  $avg_recip_mult += $legal_mults[$i] / $i;
}
$avg_recip_mult /= $nlegal;
print "$nsamples samples $nlegal legal @legal_mults avg 1/mult $avg_recip_mult\n";
my $N = 8726713169886222032347729969256422370854716254;
my $legal_frac = $nlegal / $tot_samples;
my $est = ($nlegal / $tot_samples) * $N * $avg_recip_mult;
my $conf95 = 1.96 * sqrt($legal_frac * (1-$legal_frac) / $tot_samples) * $N * $avg_recip_mult;;
printf "estimate %g +- %g at 95%% confidence\n", $est, $conf95;


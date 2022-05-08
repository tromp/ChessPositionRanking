#!/usr/bin/perl

# Let X be a random variable that is one of the N urpositions chosen uniformly at random,
# and let Y = Y(X) be a random variable that is 0 if X is illegal,
# or N/m if X is legal with multiplicity m.
# Then Y is an unbiased estimator of L, since the expected value of Y is
# E[Y] = Sum P(X legal of multiplicity m) * N/m = Sum (m * l_m/N) * N/m = Sum l_m = L
# For a sample S of n random positions containing n_S</sub> legal ones,
# we estimate L as the average of the individual estimates:
# Y(S) = (1/n) * Sum_{legal p in S} N/m(p) = (n_S/n) * N * (1/n_S) * Sum 1/m(p)

# This script requires sample size n to be given as the single command line argument
if (!@ARGV) {
  print "usage: perl estimate.pl <SAMPLE_SIZE>\n";
  exit
}
my $n_samples = $ARGV[0];

# and some set of possibly legal position on stdin, one position per line
# the first 6 fields on each line are FEN data, and the 7th is either "legal" or "illegal"
# this is typically the output of running texelutil -proofgame on a set of FENs,
# itself typically trimmed by src/legal
my $n_lines = 0;
my @legal_mults;  # array counting number of legal positions with each multiplicity
my $sum_mults = 0;
while ($line = <STDIN>) {
  @a = split / /, $line;
  if (@a <= 6) { print "not in form FEN data: $line\n"; exit(1); }
  if ($a[6] !~ /legal/) { print "no legality: $line\n"; exit(1); }
  $mult = $a[5];
  $sum_mults += $mult;
  if ($a[6] !~ /illegal/) { $legal_mults[$mult]++ };
  $n_lines++;
}
# compute the number of legal positions in the sample as the sum over array @legal_mults
# and also the sum of 1/mult over all legal positions
my $nlegal = 0;
my $sum_recip_mult = 0;
for my $i (1..$#legal_mults) {
  $nlegal += $legal_mults[$i] ;
  $sum_recip_mult += $legal_mults[$i] / $i;
}
my $mu = $sum_recip_mult / $n_samples;

# the sample mean of Y as estimate of its expected value L
printf "Exp[Y] ~ %g * N\n", $mu;

my $variance = ($n_samples - $nlegal) * (0 - $mu)**2;
for my $i (1..$#legal_mults) {
  $variance += $legal_mults[$i] * (1/$i - $mu)**2;
}
# unbiased sample variance as estimate of variance of Y
$variance /= $n_samples - 1;

printf "Var[Y] ~ %g * N^2\n", $variance;

# (biased) sample standard deviation as estimate of standard deviation of Y
my $std_dev = sqrt($variance);

printf "Sigma[Y] ~ %g * N\n", $std_dev;

# Average of n samples has sqrt(n) smaller standard deviation
my $sample_std_dev = $std_dev / sqrt($n_samples);

printf "Sigma[Y[S]] ~ %g * N\n", $sample_std_dev;

my $avg_recip_mult = $sum_recip_mult / $nlegal;

print "n = $n_samples samples $nlegal/$n_lines legal @legal_mults avg 1/mult $avg_recip_mult\n";

# Chess Position Ranking size
my $N = 8726713169886222032347729969256422370854716254;

printf "estimate %g +- %g at 95%% confidence\n", $mu * $N, 1.96 * $sample_std_dev * $N;

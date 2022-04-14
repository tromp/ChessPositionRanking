all:	count test1k

count:	Makefile
	$(MAKE) -C src/ countChess
	time src/countChess
	
noproms:	Makefile
	$(MAKE) -C src/ noproms
	time src/noproms
	
test100:	Makefile testRnd1kResearch
	diff testRnd100Research sortedRnd100Research

sortedRnd100kLegalFENs:	Makefile
	$(MAKE) -C src/ randomFENs
	$(MAKE) -C src/ sortFENs
	time src/randomFENs 100 100000 | src/sortFENs > sortedRnd100kLegalFENs

sortedRnd100kLegalRanks:	Makefile sortedRnd100kLegalFENs
	$(MAKE) -C src/ cpr
	time src/cpr rank < sortedRnd100kLegalFENs > sortedRnd100kLegalRanks

testRnd100kLegalFENs:	Makefile sortedRnd100kLegalRanks
	$(MAKE) -C src/ cpr
	time src/cpr unrank < sortedRnd100kLegalRanks > testRnd100kLegalFEN
	diff sortedRnd100kLegalFENs testRnd100kLegalFEN

sortedRnd1mLegalFENs:	Makefile
	$(MAKE) -C src/ randomFENs
	$(MAKE) -C src/ sortFENs
	time src/randomFENs 100 1000000 | src/sortFENs > sortedRnd1mLegalFENs

sortedRnd1mLegalRanks:	Makefile sortedRnd1mLegalFENs
	$(MAKE) -C src/ cpr
	time src/cpr rank < sortedRnd1mLegalFENs > sortedRnd1mLegalRanks

testRnd1mLegalFENs:	Makefile sortedRnd1mLegalRanks
	$(MAKE) -C src/ cpr
	time src/cpr unrank < sortedRnd1mLegalRanks > testRnd1mLegalFEN
	diff sortedRnd1mLegalFENs testRnd1mLegalFEN

testRnd100Ranks:	Makefile
	$(MAKE) -C src/ randomRs
	src/randomRs 8726713169886222032347729969256422370854716254 100 | sort -n > testRnd100Ranks
	diff testRnd100Ranks sortedRnd100Ranks

testRnd100FENs:	Makefile testRnd100Ranks
	$(MAKE) -C src/ cpr
	time src/cpr unrank < testRnd100Ranks > testRnd100FENs
	diff testRnd100FENs sortedRnd100FENs

testRnd100Research:	Makefile testRnd100FENs
	$(MAKE) -C src/ legal
	src/legal < testRnd100FENs > testRnd100Research
	diff testRnd100Research sortedRnd100Research
	
testRnd100Ranking:	Makefile testRnd100FENs
	$(MAKE) -C src/ cpr
	time src/cpr rank < testRnd100FENs > testRnd100FENsRanked
	diff testRnd100Ranks testRnd100FENsRanked

	
test1k:	Makefile testRnd1kResearch
	diff testRnd1kResearch sortedRnd1kResearch

testRnd1kRanks:	Makefile
	$(MAKE) -C src/ randomRs
	src/randomRs 8726713169886222032347729969256422370854716254 1000 | sort -n > testRnd1kRanks
	diff testRnd1kRanks sortedRnd1kRanks

testRnd1kFENs:	Makefile testRnd1kRanks
	$(MAKE) -C src/ cpr
	time src/cpr unrank < testRnd1kRanks > testRnd1kFENs
	diff testRnd1kFENs sortedRnd1kFENs

testRnd1kResearch:	Makefile testRnd1kFENs
	$(MAKE) -C src/ legal
	src/legal < testRnd1kFENs > testRnd1kResearch
	diff testRnd1kResearch sortedRnd1kResearch
	
testRnd1kRanking:	Makefile testRnd1kFENs
	$(MAKE) -C src/ cpr
	time src/cpr rank < testRnd1kFENs > testRnd1kFENsRanked
	diff testRnd1kRanks testRnd1kFENsRanked
	

testRnd10kRanks:	Makefile
	$(MAKE) -C src/ randomRs
	src/randomRs 8726713169886222032347729969256422370854716254 10000 | sort -n > testRnd10kRanks
	diff testRnd10kRanks sortedRnd10kRanks

testRnd10kFENs:	Makefile testRnd10kRanks
	$(MAKE) -C src/ cpr
	time src/cpr unrank < testRnd10kRanks > testRnd10kFENs
	diff testRnd10kFENs sortedRnd10kFENs

testRnd10kResearch:	Makefile testRnd10kFENs
	$(MAKE) -C src/ legal
	src/legal < testRnd10kFENs > testRnd10kResearch
	diff testRnd10kResearch sortedRnd10kResearch
	

testRnd100kRanks:	Makefile
	$(MAKE) -C src/ randomRs
	src/randomRs 8726713169886222032347729969256422370854716254 100000 | sort -n > testRnd100kRanks
	diff testRnd100kRanks sortedRnd100kRanks

testRnd100kRanking:	Makefile testRnd100kFENs
	$(MAKE) -C src/ cpr
	time src/cpr rank < testRnd100kFENs > testRnd100kFENsRanked
	diff testRnd100kRanks testRnd100kFENsRanked

testRnd100kFENs:	Makefile testRnd100kRanks
	$(MAKE) -C src/ cpr
	time src/cpr unrank < testRnd100kRanks > testRnd100kFENs
	diff testRnd100kFENs sortedRnd100kFENs

testRnd100kResearch:	Makefile testRnd100kFENs
	$(MAKE) -C src/ legal
	src/legal < testRnd100kFENs > testRnd100kResearch
	diff testRnd100kResearch sortedRnd100kResearch
	

testRnd1mRanks:	Makefile
	$(MAKE) -C src/ randomRs
	src/randomRs 8726713169886222032347729969256422370854716254 1000000 | sort -n > testRnd1mRanks
	diff testRnd1mRanks sortedRnd1mRanks

testRnd1mRanking:	Makefile testRnd1mFENs
	$(MAKE) -C src/ cpr
	time src/cpr rank < testRnd1mFENs > testRnd1mFENsRanked
	diff testRnd1mRanks testRnd1mFENsRanked
	
testRnd1mFENs:	Makefile testRnd1mRanks
	$(MAKE) -C src/ cpr
	time src/cpr unrank < testRnd1mRanks > testRnd1mFENs
	diff testRnd1mFENs sortedRnd1mFENs

testRnd1mResearch:	Makefile testRnd1mFENs
	$(MAKE) -C src/ legal
	src/legal < testRnd1mFENs > testRnd1mResearch
	diff testRnd1mResearch sortedRnd1mResearch

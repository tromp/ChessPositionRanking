all:	count test1k

count:	Makefile
	$(MAKE) -C src/ CountChess
	time src/CountChess
	
test1k:	Makefile testRnd1kResearch
	diff testRnd1kResearch sortedRnd1kResearch

testRnd1kRanks: Makefile
	$(MAKE) -C src/ randomRs
	src/randomRs 8726713169886222032347729969256422370854716254 1000 | sort -n > testRnd1kRanks
	diff testRnd1kRanks sortedRnd1kRanks

testRnd1kFENs:	Makefile testRnd1kRanks
	$(MAKE) -C src/ cpr
	time src/cpr unrank < testRnd1kRanks > testRnd1kFENs
	diff testRnd1kFENs sortedRnd1kFENs

testRnd1kResearch:	Makefile testRnd1kFENs
	$(MAKE) -C src/ legal
	src/legal -v < testRnd1kFENs > testRnd1kResearch
	diff testRnd1kResearch sortedRnd1kResearch
	
testRnd1kRanking:	Makefile testRnd1kFENs
	$(MAKE) -C src/ cpr
	time src/cpr rank < testRnd1kFENs > testRnd1kFENsRanked
	diff testRnd1kFENs testRnd1kFENsRanked
	

testRnd10kRanks: Makefile
	$(MAKE) -C src/ randomRs
	src/randomRs 8726713169886222032347729969256422370854716254 10000 | sort -n > testRnd10kRanks
	diff testRnd10kRanks sortedRnd10kRanks

testRnd10kFENs:	Makefile testRnd10kRanks
	$(MAKE) -C src/ cpr
	time src/cpr unrank < testRnd10kRanks > testRnd10kFENs
	diff testRnd10kFENs sortedRnd10kFENs

testRnd10kResearch:	Makefile testRnd10kFENs
	$(MAKE) -C src/ legal
	src/legal -v < testRnd10kFENs > testRnd10kResearch
	diff testRnd10kResearch sortedRnd10kResearch
	

testRnd100kRanks: Makefile
	$(MAKE) -C src/ randomRs
	src/randomRs 8726713169886222032347729969256422370854716254 100000 | sort -n > testRnd100kRanks
	diff testRnd100kRanks sortedRnd100kRanks

testRnd10kRanking:	Makefile testRnd10kFENs
	$(MAKE) -C src/ cpr
	time src/cpr rank < testRnd10kFENs > testRnd10kFENsRanked
	diff testRnd10kFENs testRnd10kFENsRanked
	

testRnd100kFENs:	Makefile testRnd100kRanks
	$(MAKE) -C src/ cpr
	time src/cpr unrank < testRnd100kRanks > testRnd100kFENs
	diff testRnd100kFENs sortedRnd100kFENs

testRnd100kResearch:	Makefile testRnd100kFENs
	$(MAKE) -C src/ legal
	src/legal -v < testRnd100kFENs > testRnd100kResearch
	
testRnd100kRanking:	Makefile testRnd100kFENs
	$(MAKE) -C src/ cpr
	time src/cpr rank < testRnd100kFENs > testRnd100kFENsRanked
	diff testRnd100kFENs testRnd100kFENsRanked

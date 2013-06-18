# vi:set noexpandtab:

DBG=/Users/dev/env/fpco742/local/lib/ghc-7.4.2.9/package.conf.d
DBU=/Users/dev/.ghc/x86_64-darwin-7.4.2.9/package.conf.d
DB1=/Users/dev/.cabal/db1
DB2=/Users/dev/.cabal/db2

GHC=ghc -fforce-recomp

all: test-db-order-G test-db-order-GU test-db-order-UG test-db-order-G1 test-db-order-1G test-db-order-GU1 test-db-order-UG1 test-db-order-1UG test-db-order-U1G test-db-order-1GU test-db-order-G1U test-db-order-G2 test-db-order-2G test-db-order-GU2 test-db-order-UG2 test-db-order-2UG test-db-order-U2G test-db-order-2GU test-db-order-G2U test-db-order-G12 test-db-order-1G2 test-db-order-21G test-db-order-12G test-db-order-2G1 test-db-order-G21 test-db-order-GU12 test-db-order-UG12 test-db-order-1UG2 test-db-order-U1G2 test-db-order-1GU2 test-db-order-G1U2 test-db-order-21UG test-db-order-12UG test-db-order-1U2G test-db-order-2U1G test-db-order-U21G test-db-order-U12G test-db-order-2GU1 test-db-order-G2U1 test-db-order-GU21 test-db-order-2UG1 test-db-order-U2G1 test-db-order-UG21 test-db-order-2G1U test-db-order-G21U test-db-order-G12U test-db-order-21GU test-db-order-12GU test-db-order-1G2U 

run: all
	./test-db-order-G
	./test-db-order-GU
	./test-db-order-UG
	./test-db-order-G1
	./test-db-order-1G
	./test-db-order-GU1
	./test-db-order-UG1
	./test-db-order-1UG
	./test-db-order-U1G
	./test-db-order-1GU
	./test-db-order-G1U
	./test-db-order-G2
	./test-db-order-2G
	./test-db-order-GU2
	./test-db-order-UG2
	./test-db-order-2UG
	./test-db-order-U2G
	./test-db-order-2GU
	./test-db-order-G2U
	./test-db-order-G12
	./test-db-order-1G2
	./test-db-order-21G
	./test-db-order-12G
	./test-db-order-2G1
	./test-db-order-G21
	./test-db-order-GU12
	./test-db-order-UG12
	./test-db-order-1UG2
	./test-db-order-U1G2
	./test-db-order-1GU2
	./test-db-order-G1U2
	./test-db-order-21UG
	./test-db-order-12UG
	./test-db-order-1U2G
	./test-db-order-2U1G
	./test-db-order-U21G
	./test-db-order-U12G
	./test-db-order-2GU1
	./test-db-order-G2U1
	./test-db-order-GU21
	./test-db-order-2UG1
	./test-db-order-U2G1
	./test-db-order-UG21
	./test-db-order-2G1U
	./test-db-order-G21U
	./test-db-order-G12U
	./test-db-order-21GU
	./test-db-order-12GU
	./test-db-order-1G2U

test-db-order-G:
	GHC_PACKAGE_PATH=${DBG} ${GHC} -o test-db-order-G test-db-order.hs

test-db-order-GU:
	GHC_PACKAGE_PATH=${DBG}:${DBU} ${GHC} -o test-db-order-GU test-db-order.hs

test-db-order-UG:
	GHC_PACKAGE_PATH=${DBU}:${DBG} ${GHC} -o test-db-order-UG test-db-order.hs

test-db-order-G1:
	GHC_PACKAGE_PATH=${DBG}:${DB1} ${GHC} -o test-db-order-G1 test-db-order.hs

test-db-order-1G:
	GHC_PACKAGE_PATH=${DB1}:${DBG} ${GHC} -o test-db-order-1G test-db-order.hs

test-db-order-GU1:
	GHC_PACKAGE_PATH=${DBG}:${DBU}:${DB1} ${GHC} -o test-db-order-GU1 test-db-order.hs

test-db-order-UG1:
	GHC_PACKAGE_PATH=${DBU}:${DBG}:${DB1} ${GHC} -o test-db-order-UG1 test-db-order.hs

test-db-order-1UG:
	GHC_PACKAGE_PATH=${DB1}:${DBU}:${DBG} ${GHC} -o test-db-order-1UG test-db-order.hs

test-db-order-U1G:
	GHC_PACKAGE_PATH=${DBU}:${DB1}:${DBG} ${GHC} -o test-db-order-U1G test-db-order.hs

test-db-order-1GU:
	GHC_PACKAGE_PATH=${DB1}:${DBG}:${DBU} ${GHC} -o test-db-order-1GU test-db-order.hs

test-db-order-G1U:
	GHC_PACKAGE_PATH=${DBG}:${DB1}:${DBU} ${GHC} -o test-db-order-G1U test-db-order.hs

test-db-order-G2:
	GHC_PACKAGE_PATH=${DBG}:${DB2} ${GHC} -o test-db-order-G2 test-db-order.hs

test-db-order-2G:
	GHC_PACKAGE_PATH=${DB2}:${DBG} ${GHC} -o test-db-order-2G test-db-order.hs

test-db-order-GU2:
	GHC_PACKAGE_PATH=${DBG}:${DBU}:${DB2} ${GHC} -o test-db-order-GU2 test-db-order.hs

test-db-order-UG2:
	GHC_PACKAGE_PATH=${DBU}:${DBG}:${DB2} ${GHC} -o test-db-order-UG2 test-db-order.hs

test-db-order-2UG:
	GHC_PACKAGE_PATH=${DB2}:${DBU}:${DBG} ${GHC} -o test-db-order-2UG test-db-order.hs

test-db-order-U2G:
	GHC_PACKAGE_PATH=${DBU}:${DB2}:${DBG} ${GHC} -o test-db-order-U2G test-db-order.hs

test-db-order-2GU:
	GHC_PACKAGE_PATH=${DB2}:${DBG}:${DBU} ${GHC} -o test-db-order-2GU test-db-order.hs

test-db-order-G2U:
	GHC_PACKAGE_PATH=${DBG}:${DB2}:${DBU} ${GHC} -o test-db-order-G2U test-db-order.hs

test-db-order-G12:
	GHC_PACKAGE_PATH=${DBG}:${DB1}:${DB2} ${GHC} -o test-db-order-G12 test-db-order.hs

test-db-order-1G2:
	GHC_PACKAGE_PATH=${DB1}:${DBG}:${DB2} ${GHC} -o test-db-order-1G2 test-db-order.hs

test-db-order-21G:
	GHC_PACKAGE_PATH=${DB2}:${DB1}:${DBG} ${GHC} -o test-db-order-21G test-db-order.hs

test-db-order-12G:
	GHC_PACKAGE_PATH=${DB1}:${DB2}:${DBG} ${GHC} -o test-db-order-12G test-db-order.hs

test-db-order-2G1:
	GHC_PACKAGE_PATH=${DB2}:${DBG}:${DB1} ${GHC} -o test-db-order-2G1 test-db-order.hs

test-db-order-G21:
	GHC_PACKAGE_PATH=${DBG}:${DB2}:${DB1} ${GHC} -o test-db-order-G21 test-db-order.hs

test-db-order-GU12:
	GHC_PACKAGE_PATH=${DBG}:${DBU}:${DB1}:${DB2} ${GHC} -o test-db-order-GU12 test-db-order.hs

test-db-order-UG12:
	GHC_PACKAGE_PATH=${DBU}:${DBG}:${DB1}:${DB2} ${GHC} -o test-db-order-UG12 test-db-order.hs

test-db-order-1UG2:
	GHC_PACKAGE_PATH=${DB1}:${DBU}:${DBG}:${DB2} ${GHC} -o test-db-order-1UG2 test-db-order.hs

test-db-order-U1G2:
	GHC_PACKAGE_PATH=${DBU}:${DB1}:${DBG}:${DB2} ${GHC} -o test-db-order-U1G2 test-db-order.hs

test-db-order-1GU2:
	GHC_PACKAGE_PATH=${DB1}:${DBG}:${DBU}:${DB2} ${GHC} -o test-db-order-1GU2 test-db-order.hs

test-db-order-G1U2:
	GHC_PACKAGE_PATH=${DBG}:${DB1}:${DBU}:${DB2} ${GHC} -o test-db-order-G1U2 test-db-order.hs

test-db-order-21UG:
	GHC_PACKAGE_PATH=${DB2}:${DB1}:${DBU}:${DBG} ${GHC} -o test-db-order-21UG test-db-order.hs

test-db-order-12UG:
	GHC_PACKAGE_PATH=${DB1}:${DB2}:${DBU}:${DBG} ${GHC} -o test-db-order-12UG test-db-order.hs

test-db-order-1U2G:
	GHC_PACKAGE_PATH=${DB1}:${DBU}:${DB2}:${DBG} ${GHC} -o test-db-order-1U2G test-db-order.hs

test-db-order-2U1G:
	GHC_PACKAGE_PATH=${DB2}:${DBU}:${DB1}:${DBG} ${GHC} -o test-db-order-2U1G test-db-order.hs

test-db-order-U21G:
	GHC_PACKAGE_PATH=${DBU}:${DB2}:${DB1}:${DBG} ${GHC} -o test-db-order-U21G test-db-order.hs

test-db-order-U12G:
	GHC_PACKAGE_PATH=${DBU}:${DB1}:${DB2}:${DBG} ${GHC} -o test-db-order-U12G test-db-order.hs

test-db-order-2GU1:
	GHC_PACKAGE_PATH=${DB2}:${DBG}:${DBU}:${DB1} ${GHC} -o test-db-order-2GU1 test-db-order.hs

test-db-order-G2U1:
	GHC_PACKAGE_PATH=${DBG}:${DB2}:${DBU}:${DB1} ${GHC} -o test-db-order-G2U1 test-db-order.hs

test-db-order-GU21:
	GHC_PACKAGE_PATH=${DBG}:${DBU}:${DB2}:${DB1} ${GHC} -o test-db-order-GU21 test-db-order.hs

test-db-order-2UG1:
	GHC_PACKAGE_PATH=${DB2}:${DBU}:${DBG}:${DB1} ${GHC} -o test-db-order-2UG1 test-db-order.hs

test-db-order-U2G1:
	GHC_PACKAGE_PATH=${DBU}:${DB2}:${DBG}:${DB1} ${GHC} -o test-db-order-U2G1 test-db-order.hs

test-db-order-UG21:
	GHC_PACKAGE_PATH=${DBU}:${DBG}:${DB2}:${DB1} ${GHC} -o test-db-order-UG21 test-db-order.hs

test-db-order-2G1U:
	GHC_PACKAGE_PATH=${DB2}:${DBG}:${DB1}:${DBU} ${GHC} -o test-db-order-2G1U test-db-order.hs

test-db-order-G21U:
	GHC_PACKAGE_PATH=${DBG}:${DB2}:${DB1}:${DBU} ${GHC} -o test-db-order-G21U test-db-order.hs

test-db-order-G12U:
	GHC_PACKAGE_PATH=${DBG}:${DB1}:${DB2}:${DBU} ${GHC} -o test-db-order-G12U test-db-order.hs

test-db-order-21GU:
	GHC_PACKAGE_PATH=${DB2}:${DB1}:${DBG}:${DBU} ${GHC} -o test-db-order-21GU test-db-order.hs

test-db-order-12GU:
	GHC_PACKAGE_PATH=${DB1}:${DB2}:${DBG}:${DBU} ${GHC} -o test-db-order-12GU test-db-order.hs

test-db-order-1G2U:
	GHC_PACKAGE_PATH=${DB1}:${DBG}:${DB2}:${DBU} ${GHC} -o test-db-order-1G2U test-db-order.hs


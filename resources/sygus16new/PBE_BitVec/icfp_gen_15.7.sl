(set-logic BV)

(define-fun shr1 ((x (BitVec 64))) (BitVec 64) (bvlshr x #x0000000000000001))
(define-fun shr4 ((x (BitVec 64))) (BitVec 64) (bvlshr x #x0000000000000004))
(define-fun shr16 ((x (BitVec 64))) (BitVec 64) (bvlshr x #x0000000000000010))
(define-fun shl1 ((x (BitVec 64))) (BitVec 64) (bvshl x #x0000000000000001))
(define-fun if0 ((x (BitVec 64)) (y (BitVec 64)) (z (BitVec 64))) (BitVec 64) (ite (= x #x0000000000000001) y z))

(synth-fun f ( (x (BitVec 64))) (BitVec 64)
(

(Start (BitVec 64) (#x0000000000000000 #x0000000000000001 x (bvnot Start)
                    (shl1 Start)
 		    (shr1 Start)
		    (shr4 Start)
		    (shr16 Start)
		    (bvand Start Start)
		    (bvor Start Start)
		    (bvxor Start Start)
		    (bvadd Start Start)
		    (if0 Start Start Start)
 ))
)
)
(constraint (= (f #xCA125D34F769A356) #xCA1293229A34ABEC))
(constraint (= (f #xFF6238637EC77BF0) #xFF6239014663FD28))
(constraint (= (f #x028A67D5CF56EC0C) #x028B654B67811CB5))
(constraint (= (f #x27FF7ED5BCF084F8) #x280056D63E1AC807))
(constraint (= (f #x456017C6D8D1AEF2) #x4560D266C10AD620))
(constraint (= (f #xAAAAAAAAAAAAAAAA) #xAAABAAAAAAAAAAA9))
(constraint (= (f #x00000000001BF8A4) #x00010000001BF888))
(constraint (= (f #x000000000017B3F2) #x000100000017B3DA))
(constraint (= (f #x00000000001815D2) #x00010000001815B9))
(constraint (= (f #x0000000000162760) #x0001000000162749))
(constraint (= (f #x00000000001ACC06) #x00010000001ACBEB))
(constraint (= (f #x4F2706EC411D0245) #x009E4E0DD8823A04))
(constraint (= (f #x122AC79CFD1C3B47) #x0024558F39FA3876))
(constraint (= (f #x2925FA8FB3AEEFC3) #x00524BF51F675DDE))
(constraint (= (f #x0205D1F521F60A4F) #x00040BA3EA43EC14))
(constraint (= (f #xAF94BDBCA7FC2921) #x015F297B794FF852))
(constraint (= (f #x8659E20656B4FB9B) #x010CB3C40CAD69F6))
(constraint (= (f #x1220F68F93D841D3) #x002441ED1F27B082))
(constraint (= (f #x92A65316C67B3337) #x01254CA62D8CF666))
(constraint (= (f #x5AA864F950655017) #x00B550C9F2A0CAA0))
(constraint (= (f #x956036A170198E9B) #x012AC06D42E0331C))
(constraint (= (f #x00000000001D8323) #x0000000000000000))
(constraint (= (f #x00000000001D44AF) #x0000000000000000))
(constraint (= (f #x00000000001FDDC5) #x0000000000000000))
(constraint (= (f #x00000000001B1BCF) #x0000000000000000))
(constraint (= (f #x0000000000176F61) #x0000000000000000))
(constraint (= (f #x000000000017A299) #x0000000000000000))
(constraint (= (f #x0000000000124CBB) #x0000000000000000))
(constraint (= (f #x000000000011A6FB) #x0000000000000000))
(constraint (= (f #x000000000015E19B) #x0000000000000000))
(constraint (= (f #x000000000018D21D) #x0000000000000000))
(constraint (= (f #xC36EAEB6009DDD05) #x0186DD5D6C013BBA))
(constraint (= (f #x779AF52232713DB4) #x779B7D873D4F0B42))
(constraint (= (f #x895EBBB671304A4B) #x0112BD776CE26094))
(constraint (= (f #xABD3409E763FE805) #x0157A6813CEC7FD0))
(constraint (= (f #xC6A88C69562E781B) #x018D5118D2AC5CF0))
(constraint (= (f #xBD4E50C9BE427102) #xBD4E937B6D78B2BF))
(constraint (= (f #xD975EAE7948E8F33) #x01B2EBD5CF291D1E))
(constraint (= (f #x0D03F4DEFC11F777) #x001A07E9BDF823EE))
(constraint (= (f #x8A6D9FD8D4D273FE) #x8A6E156B34F99F2B))
(constraint (= (f #xB9F5EE31B2C144A7) #x0173EBDC63658288))
(constraint (= (f #x00000000001B1D29) #x0000000000000000))
(constraint (= (f #xAAAAAAAAAAAAAAAA) #xAAABAAAAAAAAAAA9))
(constraint (= (f #x00000000001297F3) #x0000000000000000))
(constraint (= (f #xFDDEDDB5B56B56AA) #xFDDEDFD6D7B5A13E))
(constraint (= (f #xF5BBFD7BED6EDD5E) #xF5BC07BFEFF2EFEF))
(constraint (= (f #xEFB6FAEDAFAED56A) #xEFB70B36B4C125BB))
(constraint (= (f #x00000000001118F8) #x00010000001118E6))
(check-synth)

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
(constraint (= (f #xC2EAE1BB7EAE6980) #x85D5C376FD5CD302))
(constraint (= (f #xE913EECF579C4D7C) #xD227DD9EAF389AFA))
(constraint (= (f #x4AA43B43181153FE) #x954876863022A7FE))
(constraint (= (f #x2221A0AB95FE8E7C) #x444341572BFD1CFA))
(constraint (= (f #xD06CF7A6C7E31FF6) #xA0D9EF4D8FC63FEE))
(constraint (= (f #xBBB1014A0A6D2FCB) #xBBB1014A0A6D2FCB))
(constraint (= (f #xA778DCA6C2785325) #xA778DCA6C2785325))
(constraint (= (f #x8EC20A5CA0FB16CB) #x8EC20A5CA0FB16CB))
(constraint (= (f #x4E6FE234F430BBE3) #x4E6FE234F430BBE3))
(constraint (= (f #x18BA9149F9ECDF8D) #x18BA9149F9ECDF8D))
(constraint (= (f #x0000000000000002) #x0000000000000006))
(constraint (= (f #x0000000000000006) #x000000000000000E))
(constraint (= (f #x000000000000000C) #x000000000000001A))
(constraint (= (f #x0000000000000004) #x000000000000000A))
(constraint (= (f #x0000000000000000) #x0000000000000002))
(constraint (= (f #xFFFFFFFFFFFFFFFE) #xFFFFFFFFFFFFFFFC))
(constraint (= (f #x0000000000000003) #x0000000000000001))
(constraint (= (f #x000000000000000B) #x0000000000000001))
(constraint (= (f #x0000000000000001) #x0000000000000001))
(constraint (= (f #x0000000000000007) #x0000000000000001))
(constraint (= (f #x000000000000000F) #x0000000000000001))
(constraint (= (f #x5A69A08250892739) #x5A69A08250892739))
(constraint (= (f #x14A9441720011C91) #x14A9441720011C91))
(constraint (= (f #x6A11A1D3CAB50E11) #x6A11A1D3CAB50E11))
(constraint (= (f #x980731AF324C2321) #x980731AF324C2321))
(constraint (= (f #x87B1641F14034DD1) #x87B1641F14034DD1))
(constraint (= (f #x6D1E607710691396) #xDA3CC0EE20D2272E))
(constraint (= (f #x675AECBAE08B4A14) #xCEB5D975C116942A))
(constraint (= (f #xBE0160EB542AA373) #xBE0160EB542AA373))
(constraint (= (f #x253AC2EA42E1A49E) #x4A7585D485C3493E))
(constraint (= (f #x4238C39857AD3006) #x84718730AF5A600E))
(constraint (= (f #x6CB09733CE90D037) #x6CB09733CE90D037))
(constraint (= (f #xAC3C44AA716920EE) #x58788954E2D241DE))
(constraint (= (f #x98EC5D22BD30B4D2) #x31D8BA457A6169A6))
(constraint (= (f #xBC4955E77A7EB8B6) #x7892ABCEF4FD716E))
(constraint (= (f #x993EEE07446E8A7E) #x327DDC0E88DD14FE))
(constraint (= (f #x000000000000000B) #x0000000000000001))
(constraint (= (f #xFFFFFFFFFFFFFFFE) #xFFFFFFFFFFFFFFFC))
(constraint (= (f #x0000000000000008) #x0000000000000012))
(constraint (= (f #x800000000000000B) #x800000000000000B))
(constraint (= (f #x0000000000000003) #x0000000000000001))
(constraint (= (f #x0000000000000007) #x0000000000000001))
(constraint (= (f #x8C1B6EF630504885) #x8C1B6EF630504885))
(check-synth)

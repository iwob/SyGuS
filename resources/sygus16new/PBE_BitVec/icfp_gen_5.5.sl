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
(constraint (= (f #x206793AA057ECA7A) #x0000206793AA057E))
(constraint (= (f #x22A87B8525816632) #x000022A87B852581))
(constraint (= (f #xFE9750F0B6E3B7F6) #x0000FE9750F0B6E3))
(constraint (= (f #x55E1D1420FF94090) #x000055E1D1420FF9))
(constraint (= (f #x52D48491945BFE7A) #x000052D48491945B))
(constraint (= (f #x7EC8DAB011113749) #x03F646D5808889BA))
(constraint (= (f #x1B5CCFA9C3B19179) #x00DAE67D4E1D8C8B))
(constraint (= (f #x3939A4206481AD8D) #x01C9CD2103240D6C))
(constraint (= (f #x0C3EF89DCCF2782B) #x0061F7C4EE6793C1))
(constraint (= (f #x35F9935F07D10C1D) #x01AFCC9AF83E8860))
(constraint (= (f #x0000000000000001) #x0000000000000000))
(constraint (= (f #xFFFFFFFFFFFFFFFE) #x0000FFFFFFFFFFFF))
(constraint (= (f #x7FFFFFFFFFFC1188) #x0000000000000000))
(constraint (= (f #x7FFFFFFFFFFC164E) #x0000000000000000))
(constraint (= (f #x7FFFFFFFFFFC5D02) #x0000000000000000))
(constraint (= (f #x7FFFFFFFFFFCD61C) #x0000000000000000))
(constraint (= (f #x7FFFFFFFFFFDB578) #x0000000000000000))
(constraint (= (f #x7FFFFFFFFFFCDE81) #x03FFFFFFFFFFE6F4))
(constraint (= (f #x7FFFFFFFFFFD43A7) #x03FFFFFFFFFFEA1D))
(constraint (= (f #x7FFFFFFFFFFD905B) #x03FFFFFFFFFFEC82))
(constraint (= (f #x7FFFFFFFFFFD8DBD) #x03FFFFFFFFFFEC6D))
(constraint (= (f #x7FFFFFFFFFFDB691) #x03FFFFFFFFFFEDB4))
(constraint (= (f #x87BFB11F798903BA) #x000087BFB11F7989))
(constraint (= (f #x222E34492CE12FAD) #x011171A24967097D))
(constraint (= (f #x014777C9708ED7D4) #x0000014777C9708E))
(constraint (= (f #x3A8E10E309E3FE7D) #x01D47087184F1FF3))
(constraint (= (f #x7E56D4621E3DA266) #x00007E56D4621E3D))
(constraint (= (f #x84ABC12211CC9469) #x04255E09108E64A3))
(constraint (= (f #x27D3DD22E235D477) #x013E9EE91711AEA3))
(constraint (= (f #x0740B42F71A415F2) #x00000740B42F71A4))
(constraint (= (f #xE275E9AE75029BF8) #x0000E275E9AE7502))
(constraint (= (f #xA893A2632BAED3BB) #x05449D13195D769D))
(constraint (= (f #x7FFFFFFFFFFD4F04) #x0000000000000000))
(constraint (= (f #x7FFFFFFFFFFC2DFB) #x03FFFFFFFFFFE16F))
(constraint (= (f #xFFFFFFFFFFFFFFFE) #x0000FFFFFFFFFFFF))
(constraint (= (f #x933B4F2CA3DCB7BE) #x0000933B4F2CA3DC))
(constraint (= (f #x7A7CCD0CDCF3DA14) #x00007A7CCD0CDCF3))
(constraint (= (f #x0744103B2B1B25DE) #x00000744103B2B1B))
(constraint (= (f #x7A9852743C982936) #x00007A9852743C98))
(constraint (= (f #xFFFFFFFFFFFD4656) #x0000FFFFFFFFFFFD))
(constraint (= (f #x72D855E3C2005300) #x000072D855E3C200))
(constraint (= (f #x7FFFFFFFFFFC58CE) #x0000000000000000))
(constraint (= (f #x7FFFFFFFFFFD1836) #x0000000000000000))
(constraint (= (f #x47E3EA232EE4130E) #x000047E3EA232EE4))
(constraint (= (f #xF65DE7DD09B3F8EC) #x0000F65DE7DD09B3))
(constraint (= (f #x95B9F15CBED4D69E) #x000095B9F15CBED4))
(constraint (= (f #x17DAE08862B74C3C) #x000017DAE08862B7))
(check-synth)

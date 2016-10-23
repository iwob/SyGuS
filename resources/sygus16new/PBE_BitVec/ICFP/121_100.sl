
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


(constraint (= (f #x9ed167e3c9a7a343) #xfffffffffffffffe))
(constraint (= (f #x6a713397e9a2d81b) #xfffffffffffffffe))
(constraint (= (f #x4d3ce7d4530e5c35) #xfffffffffffffffe))
(constraint (= (f #xd0bc4a956e73726e) #x0238ec810135a9a5))
(constraint (= (f #xe6ba64de1a0ce92b) #xfffffffffffffffe))
(constraint (= (f #xaded55cdec3d3e5e) #x00272006a72ee2f4))
(constraint (= (f #x96e1eeb215725283) #xfffffffffffffffe))
(constraint (= (f #x4ba958ea401ae6dc) #x008c105b04ff4352))
(constraint (= (f #xa25a0043b565bbd9) #xfffffffffffffffe))
(constraint (= (f #x3b9a69dee6eee61e) #x02cd451673533357))
(constraint (= (f #xb99237d879ea6007) #xfffffffffffffffe))
(constraint (= (f #x3901939ea68c28ce) #x02d3f52d7051ae1a))
(constraint (= (f #x45e4ec5b0385ee3a) #x00c74b2c4bedc736))
(constraint (= (f #x96594d678174ee7a) #x011450a15df18b35))
(constraint (= (f #xccc3dcc9401439ea) #x02aaee6a90ff0ed7))
(constraint (= (f #x842e4ab56a40eb5a) #x01ce34808104fb08))
(constraint (= (f #x0d648bed8ee4ead2) #x03a1498f25b34b02))
(constraint (= (f #x1e9e3218a8e50adb) #xfffffffffffffffe))
(constraint (= (f #x17b69dd8a9e46cbe) #x031c916658174d28))
(constraint (= (f #x26eeb2c01d1e5449) #xfffffffffffffffe))
(constraint (= (f #x64e913ea7177b830) #x014b132f05b19cde))
(constraint (= (f #xba813e50e199b7ab) #xfffffffffffffffe))
(constraint (= (f #x956b0bbb7ab766c6) #x01010b8cc9c09952))
(constraint (= (f #xd68427e592eadc71) #xfffffffffffffffe))
(constraint (= (f #xb1645a2920dc8e27) #xfffffffffffffffe))
(constraint (= (f #x3d1ad75c80e9b9cb) #xfffffffffffffffe))
(constraint (= (f #x1449c4ae63caa67e) #x030c96c8356e8055))
(constraint (= (f #x0c920ee739740d5a) #x03a927b35ad18fa0))
(constraint (= (f #x978c445260723396) #x011daccc257da6ad))
(constraint (= (f #xebc4577b006ba78b) #xfffffffffffffffe))
(constraint (= (f #x3b5e5035dc951e5b) #xfffffffffffffffe))
(constraint (= (f #xd5ee15c82ee55035) #xfffffffffffffffe))
(constraint (= (f #x9e1eb9e477a3e65e) #x017770d74d9c6f54))
(constraint (= (f #x29e68e3e940e9941) #xfffffffffffffffe))
(constraint (= (f #x42e782aae3c302e2) #x00e35de0036eebe3))
(constraint (= (f #xc06e1b2e6d2a89ae) #x02fd374a35220194))
(constraint (= (f #x1b520cca5d0cceb1) #xfffffffffffffffe))
(constraint (= (f #x5d635eb13d03ce71) #xfffffffffffffffe))
(constraint (= (f #xdcdaa80ee62ec448) #x026a401fb35632cc))
(constraint (= (f #x62d86527ee0d2d94) #x01625d425f37a225))
(constraint (= (f #xecd3cc43749d6c02) #x032a2eace989612f))
(constraint (= (f #x3e807308012041e6) #x02f1fdab9ff27cf7))
(constraint (= (f #x25507e1063b1d1a7) #xfffffffffffffffe))
(constraint (= (f #xbc0b90cae25ee9e9) #xfffffffffffffffe))
(constraint (= (f #x1075dee1479ca1e8) #x033d867370dd6877))
(constraint (= (f #xeec224c4c48a1e35) #xfffffffffffffffe))
(constraint (= (f #x13ecca1b9b71486e) #x032f2a874d49b09d))
(constraint (= (f #x0baa1e2bbce28bbb) #xfffffffffffffffe))
(constraint (= (f #xce9888de0ae64880) #x02b1599a77835499))
(constraint (= (f #x63ba0a48c8c2bb4d) #xfffffffffffffffe))
(constraint (= (f #xe28774b30c758e21) #xfffffffffffffffe))
(constraint (= (f #x4d63c248da901e67) #xfffffffffffffffe))
(constraint (= (f #x6ee4482ae4888ba5) #xfffffffffffffffe))
(constraint (= (f #x105d15516e96e5c0) #x033c630031311346))
(constraint (= (f #x65420aeb5619eae7) #xfffffffffffffffe))
(constraint (= (f #x1b4135c2aa10874a) #x0348f286e00739d8))
(constraint (= (f #xcc032357e5ecc2db) #xfffffffffffffffe))
(constraint (= (f #x6358454d6520a210) #x01685cc0a1427867))
(constraint (= (f #x7842814dc417d2b2) #x01dce1f0a6cf1e20))
(constraint (= (f #x7481ed6e0bc97ee1) #xfffffffffffffffe))
(constraint (= (f #x3377d0b7ecd6be26) #x02a99e389f2a10f6))
(constraint (= (f #x9970ceb54eac541e) #x0151bab080b02c0f))
(constraint (= (f #x33352eee2281eb07) #xfffffffffffffffe))
(constraint (= (f #x4586b1a6c9a1177a) #x00c5d0b452947319))
(constraint (= (f #x2da7b020c7519dc8) #x02245cbe7ad83566))
(constraint (= (f #xebe0da475a968bcd) #xfffffffffffffffe))
(constraint (= (f #xb42ab6a4e1e0076a) #x008e00904b777fd9))
(constraint (= (f #x08a49695a090e014) #x0398491104793b7f))
(constraint (= (f #xc9d3c84d76276e9e) #x02962e9ca1965931))
(constraint (= (f #x6c8eb6d7a3176b57) #xfffffffffffffffe))
(constraint (= (f #x390b986449481aec) #x02d38d5d4c909f43))
(constraint (= (f #xe74952c7b1206ee4) #x03589022dcb27d33))
(constraint (= (f #xadda5e1a47cdb8a2) #x0026447744dea4d8))
(constraint (= (f #x20697ca8b907eace) #x027d11e818d3df02))
(constraint (= (f #x8ebb30dd42edd592) #x01b0caba60e32605))
(constraint (= (f #x0452be68e7a783e1) #xfffffffffffffffe))
(constraint (= (f #x5c9e71e2e0191083) #xfffffffffffffffe))
(constraint (= (f #xeed6666a6e2871d7) #xfffffffffffffffe))
(constraint (= (f #xe9e2aac3399ac6cb) #xfffffffffffffffe))
(constraint (= (f #x14ad3a8d2ebc5d23) #xfffffffffffffffe))
(constraint (= (f #x0d5c6ac110c6a99d) #xfffffffffffffffe))
(constraint (= (f #x414e661cc9c69d87) #xfffffffffffffffe))
(constraint (= (f #x9e076b7a5e7b9056) #x0177d909c475cd3c))
(constraint (= (f #xd738b3b060e4a1c0) #x021ad8acbd7b4876))
(constraint (= (f #xe34e10010ae51c9b) #xfffffffffffffffe))
(constraint (= (f #xebc438675b38de47) #xfffffffffffffffe))
(constraint (= (f #x6572b8eab0dab041) #xfffffffffffffffe))
(constraint (= (f #x90e824267ae12bee) #x013b1e4e55c3720f))
(constraint (= (f #xebed0ce42e0660ce) #x030f23ab4e37d57a))
(constraint (= (f #x1a4a2db9452a6ec9) #xfffffffffffffffe))
(constraint (= (f #x7eab64d1eb93a742) #x01f0094a370d2c58))
(constraint (= (f #x204d90e16dc6e967) #xfffffffffffffffe))
(constraint (= (f #x4dcd31b47046dd6d) #xfffffffffffffffe))
(constraint (= (f #x49d90dd3340e033d) #xfffffffffffffffe))
(constraint (= (f #xe309e81ec8be408b) #xfffffffffffffffe))
(constraint (= (f #xde54b07e9e7cae5e) #x027408bdf175e834))
(constraint (= (f #x755536c9e0323c0c) #x01800292977ea6ef))
(constraint (= (f #x339b78e428017d88) #x02ad49db4e1ff1e5))
(constraint (= (f #x35ec7acee9eeee33) #xfffffffffffffffe))
(constraint (= (f #x82c005b0a8012edd) #xfffffffffffffffe))
(check-synth)

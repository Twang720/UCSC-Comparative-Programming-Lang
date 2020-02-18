#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/smalltalk/bin/gst -f
"Timmy Wang tqwang"
"Eric Mar emmar"

"GIVEN CODE"

nl := Character nl.

Character extend [
   isPrint [
      ^ (Character space < self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
]

Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]

FileStream extend [
   nl := Character nl.
   catFile [
      |count|
      count := 0.
      [self atEnd not] whileTrue: [
         |padded|
         count := count + 1.
         padded := count printPaddedWith: Character space to: 6.
         stdout << padded << ': ' << self nextLine << nl.
      ]
   ]
].

Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]

writeBitArray := [:outfile|
   |outBytes|
   outBytes := OutBits new: outfile.
   bitArray do: [:bit| outBytes writeBit: bit].
   outBytes flushByte.
].

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [ ^ theArray size.  ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]
]

Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]
   isLeaf [ ^ true ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   depthFirst: visitor prefix: string [
      visitor value: char value: string.
   ]

   postOrder: visitor [
      visitor value: char value: 0.
   ]

].

Leaf subclass: Tree [
   |left right|
   left [ ^ left ]
   right [ ^ right ]
   isLeaf [ ^ false ]

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   depthFirst: visitor prefix: string [
      left depthFirst: visitor prefix: string, '0'.
      right depthFirst: visitor prefix: string, '1'.
   ]

   postOrder: visitor [
      left postOrder: visitor.
      right postOrder: visitor.
      visitor value: self char value: 1.
   ]

].

"MAIN FUNCTION"

freqtable := ZeroArray new: 257.
encodingtable := ZeroArray new: 257.
pqueue := SortedCollection new.
argt := false.
argc := false.
argu := false.
EOF := 256.

main:= 
[
    
    (Smalltalk arguments size) = 3
    ifTrue:
    [
        |option in out|
        option := (Smalltalk arguments at: 1).
        in := FileStream open: 
            (Smalltalk arguments at: 2) mode: FileStream read.
        out := FileStream open: 
            (Smalltalk arguments at: 3) mode: FileStream write.
        
        (option = '-c') ifTrue: [argc := true].
        (option = '-u') ifTrue: [argu := true].

        (argc) ifTrue:
        [
            |tree|
            [in atEnd not] whileTrue:
            [
                |ordChar|
                ordChar := in next asInteger.
                freqtable incr: ordChar.
            ].
            freqtable incr: EOF.

            freqtable keysAndValuesDo: 
            [ :key :value |
                (value notNil and: [value > 0]) ifTrue: 
                [
                    pqueue add: (Leaf new: key count: value).
                ]
            ].                
            
            [pqueue size > 1] whileTrue:
            [
                |l r|
                l:= pqueue removeFirst.
                r:= pqueue removeFirst.
                (l count <= r count) ifTrue:
                [
                    tree:= Tree new: (l char) count: (l count + r count) left: l right: r. 
                    pqueue add: tree.
                ]
                ifFalse:
                [
                    tree:= Tree new: (r char) count: (l count + r count) left: r right: l.
                    pqueue add: tree.
                ].
            ].

            tree depthFirst: 
            [
                :char :string | encodingtable at: char put: string.
            ] prefix: ''.

            bitOutput := OutBits new: out. 

            tree postOrder: [:char :outbit|
                |count|
                count := 8.  
                outbit = 0 ifTrue: [
                    bitOutput writeBit: 0.  
                    [count > 0] whileTrue: [
                        bitOutput writeBit: (char asInteger bitAt: count). 
                        count := count - 1. ].
                        char = 0 ifTrue: [
                        bitOutput writeBit: 0.
                    ].  
                    char = EOF ifTrue: [
                        bitOutput writeBit: 1.
                    ].  
                ] ifFalse: [
                    bitOutput writeBit: 1.
                ].  
            ].  

            bitOutput writeBit: 1.

            in reset.

            [in atEnd not] whileTrue: [
                |ordChar strCt str|
                ordChar := in next asInteger.
                str := encodingtable at: ordChar. 
                strCt := 1.   
                [strCt <= str size] whileTrue: [
                    bitOutput writeBit: ((str at: strCt) asString asInteger). 
                    strCt := strCt + 1.  
                ].  
            ].  
    
            in atEnd ifTrue: [
                |ordChar strCt str|
                str := encodingtable at: EOF. 
                strCt := 1.   
                [strCt <= str size] whileTrue: [
                    bitOutput writeBit: ((str at: strCt) asString asInteger).
                    strCt := strCt + 1.
                ].  
            ].  
            bitOutput flushByte.
        ].
        (argu) ifTrue:
        [
            |inBit outBit stack done tree head|
            inBit := BitStream new: in.
            outBit := OutBits new: out.
            stack := OrderedCollection new.
            done := false.
             
            [done not] whileTrue: [
                |next|
                next := inBit nextBit.

                (next = 0) ifTrue: [
                    |count byte|
                    count := 8.
                    byte := 0.
                    [count > 0] whileTrue: [
                        next := inBit nextBit.
                        byte := byte bitAt: count put: next.
                        count := count - 1.
                    ].

                    (byte = 0) ifTrue: [
                        next := inBit nextBit.
                        (next = 1) ifTrue: [
                            byte := EOF.
                        ]
                    ].
                    stack add: (Leaf new: byte count: 0).
                ].
                
                (next = 1) ifTrue: [
                    (stack size > 1) ifTrue: [
                        |l r|
                        l := stack removeLast.
                        r := stack removeLast.
                        tree := Tree new: (l char) count: 1 left: l right: r.
                        stack add: tree.
                    ]
                    ifFalse: [
                        tree := stack removeLast.
                        done := true.
                    ].
                ]
            ].

            head := tree.

            [inBit atEnd not] whileTrue: [
                |next|
                next := inBit nextBit.

                (next = 0) ifTrue: [
                    tree := tree left.
                    tree isLeaf ifTrue: [
                        |count byte|
                        byte := tree char.
                        count := 8.
                        [count > 0] whileTrue: [
                            outBit writeBit: (byte bitAt: count). 
                            count := count - 1.
                        ].
                        tree := head.
                    ].
                ].

                (next = 1) ifTrue: [
                    tree := tree right.
                    tree isLeaf ifTrue: [
                        |count byte|
                        byte := tree char.
                        count := 8.
                        [count > 0] whileTrue: [
                            outBit writeBit: (byte bitAt: count). 
                            count := count - 1.
                        ].
                        tree := head.
                    ].
                ].
            ].
        ].
    ]
    ifFalse: 
    [
        (Smalltalk arguments size) = 2
        ifTrue:
        [
            |option in|
            option := (Smalltalk arguments at: 1).
            in := FileStream open: 
                (Smalltalk arguments at: 2) mode: FileStream read.
        
            (option = '-t') ifTrue: [argt := true].
            (option = '-c') ifTrue: [argc := true].
            (option = '-u') ifTrue: [argu := true].

            (argc | argt) ifTrue:
            [
                |tree|
                [in atEnd not] whileTrue:
                [
                    |ordChar|
                    ordChar := in next asInteger.
                    freqtable incr: ordChar.
                ].
                freqtable incr: EOF.

                freqtable keysAndValuesDo: 
                [ :key :value |
                    (value notNil and: [value > 0]) ifTrue: 
                    [
                        pqueue add: (Leaf new: key count: value).
                    ]
                ].                
                
                [pqueue size > 1] whileTrue:
                [
                    |l r|
                    l:= pqueue removeFirst.
                    r:= pqueue removeFirst.
                    (l count <= r count) ifTrue:
                    [
                        tree:= Tree new: (l char) count: (l count + r count) left: l right: r. 
                        pqueue add: tree.
                    ]
                    ifFalse:
                    [
                        tree:= Tree new: (r char) count: (l count + r count) left: r right: l.
                        pqueue add: tree.
                    ].
                ].

                tree depthFirst: 
                [
                    :char :string | encodingtable at: char put: string.
                ] prefix: ''.

                (argt) ifTrue:
                [
                   encodingtable keysAndValuesDo:
                   [ :key :value |
                       (value notNil and: [value > 0]) ifTrue:
                       [
                           |char freq|
                           char := key asCharacter.
                           freq := freqtable at: key.
                           freq := (freq printPaddedWith: $ to:  6).
                           (char isPrint) ifTrue:
                           [
                               stdout << ' ' << char << ' ' << freq << ' ' << value << nl.
                           ]
                           ifFalse:
                           [
                               (char asInteger = EOF) ifTrue:
                               [
                                   char := 'EOF'.
                                   stdout << char << freq << ' ' << value << nl.
                               ]
                               ifFalse:
                               [
                                   (char asInteger = 32) ifTrue:
                                   [
                                       stdout << 'x20' << freq << ' ' << value << nl.
                                   ]
                                   ifFalse:
                                   [
                                       stdout << 'x0A' << freq << ' ' << value << nl.
                                   ]
                               ]
                           ]
                       ].
                   ]. 
                ].

                (argc) ifTrue:
                [
                    bitOutput := OutBits new: stdout. 

                    tree postOrder: [:char :outbit|
                        |count|
                        count := 8. 
                        outbit = 0 ifTrue: [
                            bitOutput writeBit: 0. 
                            [count > 0] whileTrue: [
                                bitOutput writeBit: (char asInteger bitAt: count). 
                                count := count - 1. ].
                                char = 0 ifTrue: [
                                bitOutput writeBit: 0.
                            ].
                            char = EOF ifTrue: [
                                bitOutput writeBit: 1.
                            ]. 
                        ] ifFalse: [
                            bitOutput writeBit: 1.
                        ].
                    ]. 

                    bitOutput writeBit: 1.

                    in reset.


                    [in atEnd not] whileTrue: [
                        |ordChar strCt str|
                        ordChar := in next asInteger.
                        str := encodingtable at: ordChar. 
                        strCt := 1.   
                        [strCt <= str size] whileTrue: [
                            bitOutput writeBit: ((str at: strCt) asString asInteger). 
                            strCt := strCt + 1. 
                        ].
                    ].
    
                    in atEnd ifTrue: [
                        |ordChar strCt str|
                        str := encodingtable at: EOF. 
                        strCt := 1.   
                        [strCt <= str size] whileTrue: [
                            bitOutput writeBit: ((str at: strCt) asString asInteger).
                            strCt := strCt + 1.
                        ].
                    ].
                    bitOutput flushByte.
                    stdout << nl.
                ]. 
            ].
            (argu) ifTrue:
            [
                |inBit outBit stack done tree head|
                inBit := BitStream new: in.
                outBit := OutBits new: stdout.
                stack := OrderedCollection new.
                done := false.
                
                [done not] whileTrue: [
                    |next|
                    next := inBit nextBit.

                    (next = 0) ifTrue: [
                        |count byte|
                        count := 8.
                        byte := 0.
                        [count > 0] whileTrue: [
                            next := inBit nextBit.
                            byte := byte bitAt: count put: next.
                            count := count - 1.
                        ].

                        (byte = 0) ifTrue: [
                            next := inBit nextBit.
                            (next = 1) ifTrue: [
                                byte := EOF.
                            ]
                        ].
                        stack add: (Leaf new: byte count: 0).
                    ].
                
                    (next = 1) ifTrue: [
                        (stack size > 1) ifTrue: [
                            |l r|
                            l := stack removeLast.
                            r := stack removeLast.
                            tree := Tree new: (l char) count: 1 left: l right: r.
                            stack add: tree.
                        ]
                        ifFalse: [
                            tree := stack removeLast.
                            done := true.
                        ].
                    ]
                ].

                head := tree.

                [inBit atEnd not] whileTrue: [
                    |next|
                    next := inBit nextBit.

                    (next = 0) ifTrue: [
                        tree := tree left.
                        tree isLeaf ifTrue: [
                            |count byte|
                            byte := tree char.
                            count := 8.
                            [count > 0] whileTrue: [
                                outBit writeBit: (byte bitAt: count). 
                                count := count - 1.
                            ].
                            tree := head.
                        ].
                    ].

                    (next = 1) ifTrue: [
                        tree := tree right.
                        tree isLeaf ifTrue: [
                            |count byte|
                            byte := tree char.
                            count := 8.
                            [count > 0] whileTrue: [
                                outBit writeBit: (byte bitAt: count). 
                                count := count - 1.
                            ].
                            tree := head.
                        ].
                    ].
                ].
            ].
        ]
        ifFalse: 
        [
            stderr << 'Usage: hzip.st option input (output)'
            ObjectMemory quit: 1.
        ].
    ].
].

main value.

structure NumD = struct

    structure TC = TypeClass

    datatype ND = 
        NumD of ((TC.term -> TC.term -> TC.term)*
                (TC.term -> TC.term -> TC.term)*
                (TC.term -> TC.term))

end
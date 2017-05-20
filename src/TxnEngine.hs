module TxnEngine where

type Monetary = Int
type Quantity = Int

type Account = Int

--type Security = String
--type Currency = String

data ValueChange = 
    Debit(Monetary) | Credit(Monetary) | Zero
    deriving (Eq, Show)

isDebit :: ValueChange -> Bool
isDebit (Debit _) = True
isDebit _ = False

isCredit :: ValueChange -> Bool
isCredit (Credit _) = True
isCredit _ = False


data Split = Split {
    account :: Account,
    valueChange :: ValueChange
--    quantity :: Quantity
--    currency :: Currency
} deriving (Show)

type Transaction = [Split]

valueChanges :: Transaction -> [ValueChange]
valueChanges = fmap valueChange

--totalDebits :: [ValueChange] -> ValueChange
--totalDebits vcs = Debit (sum [m | (Debit m) <- vcs])
--
--totalCredits :: [ValueChange] -> ValueChange
--totalCredits vcs = Credit (sum [m | (Credit m) <- vcs])

isBalanced :: Transaction -> Bool
isBalanced = (Zero ==) . mconcat . valueChanges

isValid :: Transaction -> Bool
isValid txn = let vcs = valueChanges txn in
    (any isDebit vcs) && 
    (any isCredit vcs) && 
    isBalanced txn

instance Monoid ValueChange where
    mempty = Zero
    mappend = vcappend 
    -- plus default implementation of mconcat

vcappend :: ValueChange -> ValueChange -> ValueChange
vcappend Zero b = b
vcappend a Zero = a
vcappend (Debit d1) (Debit d2) = Debit(d1+d2)
vcappend (Credit c1) (Credit c2) = Credit(c1+c2)
vcappend (Credit c) (Debit d) = vcappend (Debit d) (Credit c)
vcappend (Debit db) (Credit cr) = case db `compare` cr of
    GT -> Debit(db - cr)
    EQ -> Zero
    LT -> Credit(cr - db)

accountBalance :: Account -> Book -> ValueChange
accountBalance acc txns = 
    mconcat (accountValueChanges acc txns)

accountValueChanges :: Account -> Book -> [ValueChange]
accountValueChanges acc txns = 
    let splits = concat txns
        accSplits = filter ((== acc) . account) splits
    in 
        fmap valueChange accSplits

accCash = 1
accGroceries = 2
accCreditCard = 3
accEquity = 4

t1 = [Split accEquity (Debit 1000), Split accCash (Credit 1000)]
t2 = [Split accCreditCard (Debit 4000), Split accEquity (Credit 4000)]
t3 = [Split accCash (Debit 40), Split accGroceries (Credit 40)]

book = [t1, t2, t3]

type Book = [Transaction]

bookIsValid :: Book -> Bool
bookIsValid = all isValid

data AccountSign = DebitIncreases | CreditIncreases

displayBalance :: ValueChange -> AccountSign -> Monetary
displayBalance b sign =
    case (b, sign) of
        ((Debit m), DebitIncreases) -> m
        ((Debit m), CreditIncreases) -> -m
        ((Credit m), DebitIncreases) -> -m
        ((Credit m), CreditIncreases) -> m
        (Zero, _) -> 0



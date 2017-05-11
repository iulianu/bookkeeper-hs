type Monetary = Int

type Account = Int

type Security = String
type Currency = String

data ValueChange = 
    Debit(Monetary) | Credit(Monetary)
    deriving (Show)

isDebit :: ValueChange -> Bool
isDebit (Debit _) = True
isDebit _ = False

isCredit :: ValueChange -> Bool
isCredit (Credit _) = True
isCredit _ = False


data Split = Split {
    account :: Account,
    valueChange :: ValueChange
} deriving (Show)

type Transaction = [Split]

valueChanges :: Transaction -> [ValueChange]
valueChanges = fmap valueChange

totalDebits :: [ValueChange] -> ValueChange
totalDebits vcs = Debit (sum [m | (Debit m) <- vcs])

totalCredits :: [ValueChange] -> ValueChange
totalCredits vcs = Credit (sum [m | (Credit m) <- vcs])

isBalanced :: Transaction -> Bool
isBalanced txn = let vcs = valueChanges txn in
    balance vcs == ZeroBalance

isValid :: Transaction -> Bool
isValid txn = let vcs = valueChanges txn in
    (any isDebit vcs) && 
    (any isCredit vcs) && 
    isBalanced txn

data BalanceResult =
    CreditBalance(Monetary) | DebitBalance(Monetary) | ZeroBalance
    deriving (Eq, Show)

balance :: [ValueChange] -> BalanceResult
balance vcs = balanceDbCr (totalDebits vcs) (totalCredits vcs)

balanceDbCr :: ValueChange -> ValueChange -> BalanceResult
balanceDbCr (Debit db) (Credit cr) = case db `compare` cr of
    GT -> DebitBalance(db - cr)
    EQ -> ZeroBalance
    LT -> CreditBalance(cr - db)

accountBalance :: Account -> Book -> BalanceResult
accountBalance acc txns = let vcs = accountValueChanges acc txns in
    balanceDbCr (totalDebits vcs) (totalCredits vcs)

accountValueChanges :: Account -> Book -> [ValueChange]
accountValueChanges acc txns = 
    let splits = concat txns
        accSplits = filter (\s -> account s == acc) splits
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

displayBalance :: BalanceResult -> AccountSign -> Monetary
displayBalance b sign =
    case (b, sign) of
        ((DebitBalance m), DebitIncreases) -> m
        ((DebitBalance m), CreditIncreases) -> -m
        ((CreditBalance m), DebitIncreases) -> -m
        ((CreditBalance m), CreditIncreases) -> m
        (ZeroBalance, _) -> 0



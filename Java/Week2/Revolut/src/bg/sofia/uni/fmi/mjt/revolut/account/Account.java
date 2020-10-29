package bg.sofia.uni.fmi.mjt.revolut.account;

public abstract class Account {

    private double amount;
    private String IBAN;

    public Account(String IBAN) {
        this(IBAN, 0);
    }

    public Account(String IBAN, double amount) {
        this.IBAN = IBAN;
        this.amount = amount;
    }

    public abstract String getCurrency();

    public double getAmount() {
        return amount;
    }

    public boolean withdraw(double amount){
        if(this.amount - amount >= 0){
            this.amount-=amount;
            return true;
        } else{
            return false;
        }
    }

    public boolean topUp(double amount){
        this.amount+=amount;
        return true;
    }

    public boolean equals(Account account){
        return IBAN.equals(account.IBAN);
    }

}
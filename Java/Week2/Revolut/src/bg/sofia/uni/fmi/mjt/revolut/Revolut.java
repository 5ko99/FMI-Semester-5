package bg.sofia.uni.fmi.mjt.revolut;

import bg.sofia.uni.fmi.mjt.revolut.account.Account;
import bg.sofia.uni.fmi.mjt.revolut.card.Card;

import java.time.LocalDate;

public class Revolut implements RevolutAPI {
    public static final double EXCHANGE_RATE = 1.95583;
    private static final String BANNED_URL = ".biz";
    Account[] accounts;
    Card[] cards;
    Revolut(Account[] accounts, Card[] cards){
        this.accounts = accounts;
        this.cards = cards;
    }

    private boolean checkPin(Card card, int pin){
        short pinErrors = 0;
        boolean pinCorrect;
        do{
            pinCorrect = card.checkPin(pin);
            if(!pinCorrect){
                ++pinErrors;
            }
            if(pinErrors>=3){
                card.block();
                return false;
            }
        }while (!pinCorrect);
        return true;
    }

    private boolean cardAvl(Card card){
        boolean cardAvl = false;
        for(var _card:cards){
            if(_card.equals(card)){
                cardAvl = true;
                break;
            }
        }
        return cardAvl;
    }

    private boolean compareExpDate(Card card){
        return card.getExpirationDate().isAfter(LocalDate.now());
    }

    private Account getAccount(String currency, double amount){
        Account account = null;
        for(var _account:accounts){
            if(_account.getCurrency().equals(currency)&&_account.getAmount()>=amount){
                account = _account;
                break;
            }
        }
        return account;
    }

    @Override
    public boolean pay(Card card, int pin, double amount, String currency) {
        if(!cardAvl(card)) return false;
        if(!card.getType().equals("PHYSICAL")) return false;
        if(!compareExpDate(card)) return false;
        if(card.isBlocked()) return false;
        if(!checkPin(card,pin)) return false;
        Account account = getAccount(currency,amount);
        if(account==null) return false;
        return account.withdraw(amount);
    }

    private String getDomain(String url){
        String[] splited = url.split("\\.");
        return splited[splited.length-1];
    }

    @Override
    public boolean payOnline(Card card, int pin, double amount, String currency, String shopURL) {

        if(getDomain(shopURL).equals(BANNED_URL)) return false;
        if(!cardAvl(card)) return false;
        if(!compareExpDate(card)) return false;
        if(card.isBlocked()) return false;
        if(!checkPin(card,pin)) return false;
        Account account=getAccount(currency,amount);
        if(account==null) return false;
        boolean result = account.withdraw(amount);
        if(card.getType().equals("VIRTUALONETIME")){
            card.block();
        }
        return result;
    }

    private boolean findAccount(Account account){
        boolean avl=false;
        for(var acc:accounts){
            if(acc.equals(account)){
                avl=true;
                break;
            }
        }
        return avl;
    }

    @Override
    public boolean addMoney(Account account, double amount) {
        if(!findAccount(account)) return false;
        return account.topUp(amount);
    }

    private double bgnToEur(double amount){
        return amount/ EXCHANGE_RATE;
    }

    private double eurToBgn(double amount){
        return amount* EXCHANGE_RATE;
    }

    @Override
    public boolean transferMoney(Account from, Account to, double amount) {
        if(!findAccount(from)||!findAccount(to)||from.equals(to)) return false;
        boolean successWithdraw = from.withdraw(amount);
        if(!successWithdraw) return false;
        final String FROM_CUR = from.getCurrency();
        final String TO_CUR = to.getCurrency();
        if(FROM_CUR.equals(TO_CUR)){
            to.topUp(amount);
        }else if(FROM_CUR.equals("BGN")){
            to.topUp(bgnToEur(amount));
        }else{
            to.topUp(eurToBgn(amount));
        }
        return true;
    }

    @Override
    public double getTotalAmount() {
        double overallAmount = 0;
        for(var acc:accounts){
            if(acc.getCurrency().equals("EUR")){
               overallAmount+=eurToBgn(acc.getAmount());
            }else{
                overallAmount+= acc.getAmount();
            }
        }
        return overallAmount;
    }
}

package bg.sofia.uni.fmi.mjt.revolut.card;

import java.time.LocalDate;
import java.util.Objects;

public class VirtualPermanentCard implements Card {
    private static final String TYPE = "VIRTUALPERMANENT";
    String number;
    int pin;
    LocalDate expirationDate;
    boolean blocked = false;

    public  VirtualPermanentCard(String number, int pin, LocalDate expirationDate){
        this.number = number;
        this.pin = pin;
        this.expirationDate = expirationDate;
    }

    @Override
    public String getType() {
        return TYPE;
    }

    @Override
    public LocalDate getExpirationDate() {
        return expirationDate;
    }

    @Override
    public boolean checkPin(int pin) {
        return this.pin == pin;
    }

    @Override
    public boolean isBlocked() {
        return blocked;
    }

    @Override
    public void block() {
        blocked = true;
    }

    public String getNumber(){return number; }

    public boolean equals(Card card){
        return this.number.equals(card.getNumber());
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.number);
    }
}

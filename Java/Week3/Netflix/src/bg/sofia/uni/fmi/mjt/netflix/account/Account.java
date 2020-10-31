package bg.sofia.uni.fmi.mjt.netflix.account;

import java.time.LocalDateTime;
import java.util.Objects;

public class Account {
    String username;
    LocalDateTime birthdayDate;

    Account(String username, LocalDateTime birthdayDate){
        this.username = username;
        this.birthdayDate = birthdayDate;
    }

    public String getUsername() {
        return this.username;
    }

    public LocalDateTime getBirthdayDate(){
        return this.birthdayDate;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Account account = (Account) o;
        return Objects.equals(username, account.username);
    }

    @Override
    public int hashCode() {
        return Objects.hash(username);
    }
}

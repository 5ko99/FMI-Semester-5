package bg.sofia.uni.fmi.mjt.netflix.platform;

import bg.sofia.uni.fmi.mjt.netflix.account.Account;
import bg.sofia.uni.fmi.mjt.netflix.content.Streamable;
import bg.sofia.uni.fmi.mjt.netflix.content.enums.PgRating;
import bg.sofia.uni.fmi.mjt.netflix.exceptions.*;

import java.time.LocalDateTime;

public class Netflix implements StreamingService {
    Account[] accounts;
    Streamable[] streamableContent;
    public Netflix(Account[] accounts, Streamable[] streamableContent){
        this.accounts = accounts;
        this.streamableContent = streamableContent;
    }

    /**
     * Simulates watching activity for the given user.
     *
     * @param user             the user that will watch the video. The user must be registered in the platform in order to access its contents.
     * @param videoContentName the exact name of the video content. It is expected that the provided name matches at least one video available in the platform.
     * @throws ContentUnavailableException if the content is age restricted and the user is not yet permitted to access it.
     * @throws UserNotFoundException       if the user is not registered in the platform.
     * @throws ContentNotFoundException    if the content is not present in the platform.
     */
    @Override
    public void watch(Account user, String videoContentName) throws ContentUnavailableException {
        if(!findUser(user)) throw new UserNotFoundException("User is not found!");
        Streamable s = findContent(videoContentName);
        if(s==null) throw new ContentNotFoundException("Content not found!");
        if(!ageRestriction(user,s)) throw new ContentUnavailableException("This streamable is not appropriate for this user!");
        s.
    }

    /**
     * @param videoContentName the exact name of the video content.
     * @return the Streamable resource with name that matches the provided name or null if no such content exists in the platform.
     */
    @Override
    public Streamable findByName(String videoContentName) {
        return null;
    }

    /**
     * @return the most watched Streamable resource available in the platform or null if no streams were done yet.
     */
    @Override
    public Streamable mostViewed() {
        return null;
    }

    /**
     * @return the minutes spent by all users registered in the platform while watching streamable content.
     */
    @Override
    public int totalWatchedTimeByUsers() {
        return 0;
    }

    private boolean findUser(Account user){
        for(var acc:accounts){
            if(user.equals(acc)) return true;
        }
        return false;
    }

    private Streamable findContent(String contentName){
        for (var streamable : streamableContent){
            if(contentName.equals(streamable.getTitle())) return streamable;
        }
        return null;
    }

    private boolean ageRestriction(Account user,Streamable content){
        if(content.getRating()== PgRating.G){
            return true;
        } else if(content.getRating()==PgRating.PG13){
            return LocalDateTime.now().getYear() - user.getBirthdayDate().getYear() >= 13;
        } else {
            return LocalDateTime.now().getYear() - user.getBirthdayDate().getYear() >= 17;
        }
    }
}

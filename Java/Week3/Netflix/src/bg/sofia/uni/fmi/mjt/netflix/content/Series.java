package bg.sofia.uni.fmi.mjt.netflix.content;

import bg.sofia.uni.fmi.mjt.netflix.content.enums.Genre;
import bg.sofia.uni.fmi.mjt.netflix.content.enums.PgRating;

import java.util.Objects;

public class Series implements Streamable {
    String name;
    Genre genre;
    PgRating rating;
    Episode[] episodes;
    int viewedTimes;
    public Series(String name, Genre genre, PgRating rating, Episode[] episodes){
        this.name = name;
        this.genre = genre;
        this.rating = rating;
        this.episodes = episodes;
        this.viewedTimes = 0;
    }

    @Override
    public String getTitle() {
        return name;
    }

    @Override
    public int getDuration() {
        int duration=0;
        for(var episode:episodes){
            duration+=episode.getDuration();
        }
        return duration;
    }

    @Override
    public PgRating getRating() {
        return rating;
    }

    public int getViewedTimes(){return viewedTimes;};
    private void incViews(){this.viewedTimes+=1;}

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Series series = (Series) o;
        return name.equals(series.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }
}

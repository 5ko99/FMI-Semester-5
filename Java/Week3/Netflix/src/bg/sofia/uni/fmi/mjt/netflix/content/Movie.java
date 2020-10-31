package bg.sofia.uni.fmi.mjt.netflix.content;

import bg.sofia.uni.fmi.mjt.netflix.content.enums.Genre;
import bg.sofia.uni.fmi.mjt.netflix.content.enums.PgRating;

import java.util.Objects;

public class Movie implements  Streamable {
    String name;
    Genre genre;
    PgRating rating;
    int duration;
    int viewedTimes;
    public Movie(String name, Genre genre, PgRating rating, int duration){
        this.name = name;
        this.genre = genre;
        this.rating = rating;
        this.duration = duration;
        this.viewedTimes = 0;
    }


    @Override
    public String getTitle() {
        return name;
    }

    @Override
    public int getDuration() {
        return duration;
    }

    public int getViewedTimes(){return viewedTimes;};
    private void incViews(){this.viewedTimes+=1;}

    @Override
    public PgRating getRating() {
        return rating;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Movie movie = (Movie) o;
        return name.equals(movie.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }
}

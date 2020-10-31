package bg.sofia.uni.fmi.mjt.netflix.content;

import java.util.Objects;

public class Episode {
    String name;
    int duration;
    Episode(String name, int duration){
        this.name = name;
        this.duration = duration;
    }

    public String getName(){
        return this.name;
    }

    public int getDuration(){
        return this.duration;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Episode episode = (Episode) o;
        return name.equals(episode.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }
}

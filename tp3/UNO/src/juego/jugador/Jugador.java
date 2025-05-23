package juego.jugador;

import juego.carta.Carta;
import java.util.List;

public class Jugador {

    private List<Carta> mano;
    public String nombre;

    public Jugador(String nombre, List<Carta> cartas) {
        this.mano = cartas;
        this.nombre = nombre;
    }

    public boolean jugarCarta(Carta cartaJugador, Carta cartaPozo) {
        if (cartaJugador.esCompatible(cartaPozo)){
            mano.remove(cartaJugador);
            return true;
        }
        return false;
    }

    public Jugador recibirCarta(Carta carta) {
        mano.add(carta);
        return this;
    }

    public int getManoSize() {
        return mano.size();
    }

    public boolean manoVacia() {
        return mano.isEmpty();
    }
}

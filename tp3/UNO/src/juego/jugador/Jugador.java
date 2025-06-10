package juego.jugador;

import juego.carta.Carta;
import juego.carta.CartaReverse;

import java.util.List;

public class Jugador {

    public List<Carta> mano;
    public String nombre;

    public Jugador(String nombre, List<Carta> cartas) {
        this.mano = cartas;
        this.nombre = nombre;
    }

    public boolean jugarCarta(Carta cartaJugador, Carta cartaPozo) {
        //ver que el jugador tenga la carta
        if (!mano.contains(cartaJugador)) {
            throw new IllegalArgumentException("El jugador no tiene la carta.");
        }
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

    public List<Carta> getMano() {return mano;}
}

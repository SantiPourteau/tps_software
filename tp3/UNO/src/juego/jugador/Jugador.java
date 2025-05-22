package juego.jugador;

import juego.carta.Carta;
import java.util.List;

public class Jugador {

    private List<Carta> mano;

    public Jugador(List<Carta> cartas) {
        this.mano = cartas;
    }

    public Jugador jugarCarta(Carta carta) {
        boolean removed = mano.remove(carta);
        if (!removed) {
            throw new IllegalArgumentException("La carta no está en la mano");
        }
        return this;
    }

    public void recibirCarta(Carta carta) {
        
    }

    public int getManoSize() {
    }

    public boolean manoVacia() {
    }
}

package anillo;


import java.util.NoSuchElementException;

public class EmptyRing extends Ring {
    public EmptyRing() {}

    @Override
    public Object current(){throw new NoSuchElementException("El anillo está vacío");}

    @Override
    public EmptyRing next(){throw new NoSuchElementException("El anillo está vacío");}

    @Override
    public MultiRingRing add(Object cargo){
        return new MultiRingRing(cargo);
    }

    @Override
    public EmptyRing remove(){throw new NoSuchElementException("No se puede eliminar de un anillo vacío");}
}


package anillo;

public class MultiRingRing extends Ring{
    private Object cargo;
    private MultiRingRing next;

    public MultiRingRing(Object cargo){
        this.cargo = cargo;
        this.next = this;
    }

    @Override
    public MultiRingRing current(){
        return this;
    }

    @Override
    public MultiRingRing next(){
        return this.next;
    }

    @Override
    public MultiRingRing add(Object cargo){
        MultiRingRing newNode = new MultiRingRing(cargo);
        newNode.next = this.next;
        this.next = newNode;
        Object temp = this.cargo;
        this.cargo = newNode.cargo;
        newNode.cargo = temp;
        return this;
    }

    @Override
    public MultiRingRing remove(){

    }
}

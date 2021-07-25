package nGql;

public class Condition {
    protected Object value;
    protected Condition value1;
    protected Condition value2;

    protected String encode(){
        return "";
    }
}
class Equal extends  Condition{

    public Equal(Object value) {
        super.value = value;
    }

    @Override
    protected String encode(){
        return "%s " +"=="+" "+value;
    }

}
class NE extends Condition{
    public NE(Object value) {
        super.value = value;
    }
    @Override
    protected String encode(){
        return "%s " +"!="+" "+value;
    }

}
class IsNull extends Condition{

    public IsNull() {
    }
    @Override
    protected String encode(){
        return "%s "+"is null";
    }
}
class IsNotNull extends Condition{
    public IsNotNull() {
    }
    @Override
    protected String encode(){
        return "%s "+"is not null";
    }
}
class LT extends Condition{
    public LT(Object value) {
        super.value = value;
    }
    @Override
    protected String encode(){
        return "%s "+"<"+" "+value;
    }
}
class LE extends Condition{
    public LE(Object value) {
        super.value = value;
    }
    @Override
    protected String encode(){
        return "%s "+"<="+" "+value;
    }
}
class GT extends Condition{
    public GT(Object value) {
        super.value = value;
    }
    @Override
    protected String encode(){
        return "%s "+">"+" "+value;
    }
}
class GE extends Condition{
    public GE(Object value) {
        super.value = value;
    }
    @Override
    protected String encode(){
        return "%s "+">="+" "+value;
    }
}
class StartsWith extends Condition{
    public StartsWith(Object value) {
        super.value = value;
    }
    @Override
    protected String encode(){
        return "%s "+"STARTS WITH"+""+value;
    }
}
class EndsWith extends Condition{
    public EndsWith(Object value) {
        super.value = value;
    }
    @Override
    protected String encode(){
        return "%s "+"ENDS WITH"+" "+value;
    }
}
class Contains extends Condition{
    public Contains(Object value) {
        super.value = value;
    }
    @Override
    protected String encode(){
        return "%s "+"CONTAINS"+" "+value;
    }
}
class In extends Condition{
    public In(Object value) {
        super.value = value;
    }
    @Override
    protected String encode(){
        return "%s "+"IN"+" "+value;
    }
}
class And extends Condition{

    public And(Condition value1,Condition value2) {
        super.value1 = value1;
        super.value2 = value2;
    }
    @Override
    protected String encode(){
        return value1.encode()+"and "+value2.encode() ;
    }
}
class Or extends Condition{
    public Or(Condition value1,Condition value2) {
        super.value1 = value1;
        super.value2 = value2;
    }
    @Override
    protected String encode(){
        return value1.encode()+"or "+value2.encode() ;
    }
}
class Regular extends Condition{
    public Regular(Object value) {
        this.value = value;
    }
    @Override
    protected String encode(){
        return "%s"+" =~ "+value;
    }
}




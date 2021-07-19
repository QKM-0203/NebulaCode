package operation;

import Operator.Operator;
import common.Operate;
import entity.Graph;
import ogm.Tag;

import java.lang.reflect.Field;

public class TagOperation implements Operate<Object> {
    private final Graph graph;

    public TagOperation(Graph graph ){
        this.graph = graph;
    }


    @Override
    public void create(Object Entity) throws NoSuchFieldException, IllegalAccessException {
        Tag tag = (Tag)Entity;
        Class<? extends Tag> aClass = tag.getClass();
        Field name = aClass.getDeclaredField("name");
        //拼接nGql调用graph执行run
        name.setAccessible(true);
        // System.out.println(name.get(tag));
        graph.run(Operator.CREATE+Operator.TAG+name.get(tag)+Operator.LEFT_BRACKETS+Operator.RIGHT_BRACKETS);
    }

    @Override
    public void drop(Object Entity) throws NoSuchFieldException, IllegalAccessException {
        Tag tag = (Tag)Entity;
        Class<? extends Tag> aClass = tag.getClass();
        Field name = aClass.getDeclaredField("name");
        //拼接nGql调用graph执行run
        name.setAccessible(true);
       // System.out.println(Operator.DROP+Operator.TAG +name.get(tag)+Operator.LEFT_BRACKETS+Operator.RIGHT_BRACKETS);
        graph.run(Operator.DROP+Operator.TAG +name.get(tag));

    }

    @Override
    public void update(Object Entity) {

    }
}

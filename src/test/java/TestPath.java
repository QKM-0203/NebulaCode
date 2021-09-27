/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.ngqlbuilder.entity.Path;
import com.vesoft.nebula.ngqlbuilder.entity.Segment;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;

/**
 * because the schema creation and index creation steps are implemented asynchronously,
 * the Nepal graph cannot be created until the next heartbeat cycle,
 * so you can wait and execute the method again.
 */
public class TestPath extends TestDataBase {
    @Test
    public void testCanConstructSimplePath() {
        assert path.getVertices().size() == 5;
        assert path.getRelationships().size() == 4;
        assert path.steps() == 4;
    }

    @Test
    public void testCanConstructPathWithNoneNode() {
        List<Segment> segments = new ArrayList<>();
        Segment segment12 = new Segment(vertexOne, relationship12, vertexTwo);
        Segment segment32 = new Segment(vertexTwo, relationship32, null);
        segments.add(segment12);
        segments.add(segment32);
        Path path = new Path(segments);
        assert path.getVertices().size() == 2;
        assert path.getRelationships().size() == 2;
        assert path.steps() == 2;
    }

    @Test
    public void testCanConstructPathWithNoneNodeAndRelationship() {
        List<Segment> segments = new ArrayList<>();
        Segment segment12 = new Segment(vertexOne, relationship12, vertexTwo);
        Segment segment32 = new Segment(vertexTwo, null, null);
        segments.add(segment12);
        segments.add(segment32);
        Path path = new Path(segments);
        assert path.getVertices().size() == 3;
        assert path.getRelationships().size() == 2;
    }

    @Test
    public void testRelationshipCanNotConnectNode() {
        List<Segment> segments = new ArrayList<>();
        Segment segment12 = new Segment(vertexOne, relationship12, vertexThird);
        segments.add(segment12);
        try {
            Path path = new Path(segments);
        } catch (Exception e) {
            assert e.getMessage().equals("(1)-[:team@1{teacherName: \"qkm\", "
                + "object: \"math\"}]->(2)" + " can not connect (3 :QKM2{name: \"sy\","
                + " birth: 2001-08-13 06:12:12:123, age: 20} :QKM1{})");
        }
    }

    @Test
    public void testNodeCanNotConnectRelationship() {
        List<Segment> segments = new ArrayList<>();
        Segment segment12 = new Segment(vertexOne, relationship32, vertexThird);
        segments.add(segment12);
        try {
            Path path = new Path(segments);
        } catch (Exception e) {
            assert e.getMessage().equals("(1 :QKM2{name: \"qkm\", birth: 2002-02-03 06:12:12:123,"
                + " age: 19} :QKM1{})" + " can not connect (3)-[:work@1{}]->(2)");
        }
    }

    @Test
    public void testNodeCanNotConnectSegment() {
        List<Segment> segments = new ArrayList<>();
        Segment segment12 = new Segment(vertexOne, relationship12, vertexTwo);
        Segment segment34 = new Segment(vertexThird, relationship34, vertexFour);
        segments.add(segment12);
        segments.add(segment34);
        try {
            Path path = new Path(segments);
        } catch (Exception e) {
            assert e.getMessage().equals("(2 :QKM2{name: \"sc\","
                + " birth: 2001-04-07 06:12:12:123, age: 19} :QKM1{})"
                + " can not connect Segment{startVertex=(3 :QKM2{name: \"sy\", "
                + "birth: 2001-08-13 06:12:12:123, age: 20} :QKM1{}), "
                + "endVertex=(4 :QKM2{name: \"yq\", birth: 1999-12-25 06:12:12:123, "
                + "age: 21} :QKM1{}), " + "relationship=(3)-[:work@1{}]->(4)}");
        }
    }

    @Test
    public void testPathSegment() {
        assert path.getSegments().get(0).equals(segment12);
        assert path.getSegments().get(1).equals(segment32);
        assert path.getSegments().get(2).equals(segment34);
        assert path.getSegments().get(3).equals(segment24);

    }

    @Test
    public void tesCanWalkPath() {
        ArrayList<Object> walk = new ArrayList<>();
        walk.add(vertexOne);
        walk.add(relationship12);
        walk.add(vertexTwo);
        walk.add(relationship32);
        walk.add(vertexThird);
        walk.add(relationship34);
        walk.add(vertexFour);
        walk.add(relationship24);
        walk.add(vertexTwo);
        assert path.walk().equals(walk);
    }

    @Test
    public void tesCanEqualPath() {
        List<Segment> segments = new ArrayList<>();
        Segment segment12 = new Segment(vertexOne, relationship12, vertexTwo);
        Segment segment32 = new Segment(vertexTwo, relationship32, vertexThird);
        segments.add(segment12);
        segments.add(segment32);
        Path path = new Path(segments);
        Path path1 = new Path(segments);
        assert path.equals(path1);
    }

    @Test
    public void tesCanNotEqualPath() {
        List<Segment> segments = new ArrayList<>();
        Segment segment12 = new Segment(vertexOne, relationship12, vertexTwo);
        Segment segment32 = new Segment(vertexTwo, relationship32, vertexThird);
        segments.add(segment12);
        segments.add(segment32);
        Path path = new Path(segments);
        assert !this.path.equals(path);
    }

    @Test
    public void testPathStartAndEndVertex() {
        assert path.getStartVertex().equals(vertexOne);
        assert path.getEndVertex().equals(vertexTwo);
    }

}

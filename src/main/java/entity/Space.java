package entity;

import Operator.DateType;

public class Space {

    private String spaceName;

    //the number of partitions in graph space
    private int partitionNumber = 100;

    //number of copies per slice
    private int replicaFactor = 1;

    //optional value for data type of id is FIXED_STRING(<N>)和INT64
    private DateType vidDateType = DateType.FIXED_STRING;

    public Space(String spaceName, int partitionNumber, int replicaFactor, DateType vidDateType) {
        this.spaceName = spaceName;
        this.partitionNumber = partitionNumber;
        this.replicaFactor = replicaFactor;
        this.vidDateType = vidDateType;
    }

    public Space() {

    }

    public String getSpaceName() {
        return spaceName;
    }

    public void setSpaceName(String spaceName) {
        this.spaceName = spaceName;
    }


    public void setPartitionNumber(int partitionNumber) {
        this.partitionNumber = partitionNumber;
    }


    public void setReplicaFactor(int replicaFactor) {
        this.replicaFactor = replicaFactor;
    }



    public void setVidType(DateType vidDateType) {
        this.vidDateType = vidDateType;
    }
}
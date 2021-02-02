package p1;

public class J_1 {
    public static abstract class Builder<MessageType extends GeneratedMessageLite<MessageType, BuilderType>, BuilderType extends Builder<MessageType, BuilderType>>
            extends AbstractMessageLite.Builder<MessageType, BuilderType> {

    }

    public abstract class GeneratedMessageLite<MessageType extends GeneratedMessageLite<MessageType, BuilderType>, BuilderType extends Builder<MessageType, BuilderType>>
            extends AbstractMessageLite<MessageType, BuilderType> {

    }

    public interface MessageLite
            extends MessageLiteOrBuilder {
        public static interface Builder
                extends MessageLiteOrBuilder, Cloneable {

        }

    }

    public static final class FileDescriptorProto
            extends GeneratedMessageV3
            implements FileDescriptorProtoOrBuilder {

    }

    public static interface FileDescriptorProtoOrBuilder
            extends MessageOrBuilder {

    }
    public abstract class GeneratedMessageV3
            extends AbstractMessage
            implements java.io.Serializable {
    }

    public static abstract class AbstractMessageLite<MessageType extends AbstractMessageLite<MessageType, BuilderType>, BuilderType extends Builder<MessageType, BuilderType>>
            implements MessageLite {
        public abstract class Builder<MessageType extends AbstractMessageLite<MessageType, BuilderType>, BuilderType extends Builder<MessageType, BuilderType>>
                implements MessageLite.Builder {

        }
    }

    public static abstract class AbstractMessage
            extends AbstractMessageLite
            implements Message {

    }

    public interface MessageLiteOrBuilder {
    }

    public interface Message
            extends MessageLite, MessageOrBuilder {

        public static interface Builder
                extends MessageLite.Builder, MessageOrBuilder {
        }
    }
    public interface MessageOrBuilder
            extends MessageLiteOrBuilder {

    }
}
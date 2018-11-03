package tasks

import play.api.inject.SimpleModule
import play.api.inject.bind

class TasksModule extends SimpleModule(bind[BaseActorTask].toSelf.eagerly())
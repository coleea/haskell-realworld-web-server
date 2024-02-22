{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RealWorld.Infra.Database.PGCommentRepository where

import Control.Exception.Safe (MonadMask)
import Data.Has (Has)
import Data.ULID (ULID)
import Database.PostgreSQL.Simple
  ( FromRow,
    Only (Only),
    ToRow,
    execute,
    query,
  )
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Database.PostgreSQL.Simple.Types (PGArray (..))
import RealWorld.Domain.Command.Article.Entity.Comment
import RealWorld.Domain.Command.Article.Value (CommentBody (..))
import qualified RealWorld.Infra.Component.Database as Database
import RealWorld.Infra.Database.Repo (withConnection)
import Relude
import Safe (headMay)

type Database r m = (Has Database.State r, MonadIO m, MonadState r m, MonadMask m, MonadFail m)

deriving instance ToField CommentBody

instance ToRow Comment where
  toRow Comment {..} =
    [ toField commentId,
      toField commentBody,
      toField commentCreatedAt,
      toField commentUpdatedAt,
      toField commentAuthorId,
      toField commentArticleId,
      -- for on conflict update
      toField commentBody,
      toField commentCreatedAt,
      toField commentAuthorId,
      toField commentArticleId
    ]

deriving instance FromField CommentBody

instance FromRow Comment where
  fromRow =
    Comment
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

save :: (Database r m) => Comment -> m ()
save comment =
  withConnection $ \conn ->
    liftIO
      $ void
      $ execute
        conn
        "INSERT INTO comments \
        \ (id, body, created_at, updated_at, author_id, article_id)\
        \ VALUES (?, ?, ?, ?, ?, ?)\
        \ ON CONFLICT (id) DO\
        \ UPDATE SET\
        \   body = ?,\
        \   created_at = ?,\
        \   author_id = ?,\
        \   article_id = ?,\
        \   updated_at = now()"
        comment

findById :: (Database r m) => ULID -> m (Maybe Comment)
findById commentId =
  withConnection $ \conn ->
    liftIO
      $ headMay
      <$> query
        conn
        "SELECT id, body, created_at, updated_at, author_id, article_id \
        \FROM comments WHERE id = ?"
        (Only commentId)

delete :: (Database r m) => Comment -> m ()
delete comment =
  withConnection $ \conn ->
    liftIO $ void $ execute conn "DELETE FROM comments WHERE id = ?" (Only $ commentId comment)